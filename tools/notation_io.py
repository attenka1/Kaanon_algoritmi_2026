"""MIDI / MusicXML bridge. Python standard library only; times in quarter notes."""
import csv
import math
import struct
import sys
import zipfile
from pathlib import Path
import xml.etree.ElementTree as ET
from fractions import Fraction


def lcm(a, b):
    """Compatible with Python versions preceding math.lcm (3.9)."""
    return abs(a // math.gcd(a, b) * b) if a and b else 0


def indent_xml(element, level=0):
    """Portable formatting: ElementTree.indent also requires Python 3.9."""
    prefix = '\n' + '  ' * level
    if len(element):
        if not element.text or not element.text.strip():
            element.text = prefix + '  '
        for child in element:
            indent_xml(child, level + 1)
        if not element[-1].tail or not element[-1].tail.strip():
            element[-1].tail = prefix
    if level and (not element.tail or not element.tail.strip()):
        element.tail = prefix


def table(path):
    with open(path, newline='', encoding='utf-8') as f:
        return list(csv.DictReader(f))


def save_input(folder, notes, measures, end):
    if not notes:
        raise ValueError('No notes in input')
    with open(folder/'notes.csv', 'w', newline='') as f:
        w = csv.writer(f); w.writerow(['onset','duration','pitch','voice']); w.writerows(notes)
    with open(folder/'measures.csv', 'w', newline='') as f:
        w = csv.writer(f); w.writerow(['start','length','numerator','denominator']); w.writerows(measures)
    (folder/'end.txt').write_text(str(end)+'\n')


def vlq(data, pos):
    value = 0
    for _ in range(4):
        b = data[pos]; pos += 1; value = (value << 7) | (b & 127)
        if not b & 128:
            return value, pos
    raise ValueError('Invalid MIDI variable-length number')


def midi_read(path):
    data = path.read_bytes()
    if data[:4] != b'MThd': raise ValueError('Invalid MIDI header')
    size = struct.unpack('>I', data[4:8])[0]
    fmt, count, division = struct.unpack('>HHH', data[8:14])
    if fmt == 2 or division & 0x8000: raise ValueError('MIDI format 2 / SMPTE is not supported')
    pos = 8+size; notes = []; meters = {0:(4,4)}; end = 0
    for track in range(count):
        if data[pos:pos+4] != b'MTrk': raise ValueError('Missing MIDI track')
        length = struct.unpack('>I',data[pos+4:pos+8])[0]
        chunk = data[pos+8:pos+8+length]; pos += 8+length
        p = tick = 0; running = None; active = {}
        while p < len(chunk):
            delta,p = vlq(chunk,p); tick += delta; end = max(end,tick/division)
            b = chunk[p]
            if b & 128:
                p += 1
                if b < 0xf0: running = b
            else:
                if running is None: raise ValueError('Invalid MIDI running status')
                b = running
            if b == 0xff:
                kind = chunk[p]; p += 1; length,p = vlq(chunk,p)
                payload = chunk[p:p+length]; p += length
                if kind == 0x58: meters[tick/division] = (payload[0],2**payload[1])
                running = None
            elif b in (0xf0,0xf7):
                length,p = vlq(chunk,p); p += length; running = None
            else:
                kind = b >> 4; channel = b & 15
                a = chunk[p]; p += 1
                value = chunk[p] if kind not in (0xc,0xd) else 0
                if kind not in (0xc,0xd): p += 1
                key = (channel,a)
                if kind == 9 and value:
                    if key in active: raise ValueError('Overlapping same-pitch MIDI notes')
                    active[key] = tick
                elif kind == 8 or (kind == 9 and not value):
                    if key in active:
                        start = active.pop(key)
                        if tick > start: notes.append((start/division,(tick-start)/division,a,track*16+channel+1))
                elif kind == 0xb and a == 64 and value >= 64:
                    raise ValueError('Remove sustain pedal before reading a melodic MIDI input')
        if active: raise ValueError('Unterminated MIDI notes')
    # Stable compact voice numbers, independent of empty conductor tracks.
    voices = {v:i+1 for i,v in enumerate(sorted(set(n[3] for n in notes)))}
    notes = [(a,b,c,voices[v]) for a,b,c,v in notes]
    measures = []; start = 0.; meter_times = sorted(meters)
    while start < end-1e-8:
        num,den = meters[max(t for t in meter_times if t <= start+1e-8)]
        length = num*4/den
        upcoming = [t for t in meter_times if t > start+1e-8]
        if upcoming and upcoming[0] < start+length-1e-8: length = upcoming[0]-start
        measures.append((start,length,num,den)); start += length
    return notes,measures,max(end,start)


def xml_read(path):
    if path.suffix.lower()=='.mxl':
        with zipfile.ZipFile(path) as z:
            container = ET.fromstring(z.read('META-INF/container.xml'))
            rootfile = next(e for e in container.iter() if e.tag.split('}')[-1]=='rootfile')
            root = ET.fromstring(z.read(rootfile.attrib['full-path']))
    else: root = ET.parse(path).getroot()
    for e in root.iter(): e.tag = e.tag.split('}')[-1]
    if root.tag != 'score-partwise': raise ValueError('Use score-partwise MusicXML')
    notes = []; measures = []; voice_ids = {}; score_end = 0
    for part_index,part in enumerate(root.findall('part')):
        start = 0.; divisions = 1; num = den = 4; ties = {}; chromatic = 0
        for mi,measure in enumerate(part.findall('measure')):
            attributes = measure.find('attributes')
            if attributes is not None:
                divisions = int(attributes.findtext('divisions',str(divisions)))
                num = sum(int(x) for x in attributes.findtext('time/beats',str(num)).split('+'))
                den = int(attributes.findtext('time/beat-type',str(den)))
                chromatic = int(attributes.findtext('transpose/chromatic',str(chromatic)))+12*int(attributes.findtext('transpose/octave-change','0')) if attributes.find('transpose') is not None else chromatic
            cursor = 0.; furthest = 0.; last = 0.
            for item in measure:
                tag = item.tag
                if tag in ('backup','forward'):
                    amount = int(item.findtext('duration','0'))/divisions
                    cursor += amount if tag=='forward' else -amount
                    furthest = max(furthest,cursor)
                elif tag=='note':
                    if item.find('grace') is not None: raise ValueError('Grace notes require explicit durations; remove or realize them first')
                    duration = int(item.findtext('duration','0'))/divisions
                    if duration <= 0: raise ValueError('Missing MusicXML note duration')
                    chord = item.find('chord') is not None
                    onset = last if chord else cursor
                    last = onset
                    if not chord: cursor += duration
                    furthest = max(furthest,onset+duration)
                    if item.find('rest') is not None: continue
                    pitch = item.find('pitch')
                    if pitch is None: raise ValueError('Unpitched notes are not melodic input')
                    alter = float(pitch.findtext('alter','0'))
                    if alter != int(alter): raise ValueError('Microtonal pitches are not supported')
                    p = (int(pitch.findtext('octave'))+1)*12+{'C':0,'D':2,'E':4,'F':5,'G':7,'A':9,'B':11}[pitch.findtext('step')]+int(alter)+chromatic
                    key = (part_index,item.findtext('staff','1'),item.findtext('voice','1'))
                    voice = voice_ids.setdefault(key,len(voice_ids)+1)
                    tie = {e.attrib['type'] for e in item.findall('tie')}
                    tie_key = (voice,p)
                    if 'stop' in tie:
                        if tie_key not in ties: raise ValueError('Tie stop without start')
                        ix = ties[tie_key]
                        if abs(notes[ix][0]+notes[ix][1]-(start+onset)) > 1e-7: raise ValueError('Non-contiguous tie')
                        notes[ix][1] += duration
                        if 'start' not in tie: del ties[tie_key]
                    else:
                        notes.append([start+onset,duration,p,voice])
                        if 'start' in tie: ties[tie_key] = len(notes)-1
            length = furthest if measure.attrib.get('implicit')=='yes' else max(num*4/den,furthest)
            if length<=0: length = num*4/den
            if part_index==0: measures.append((start,length,num,den))
            elif mi >= len(measures) or abs(measures[mi][0]-start)>1e-7 or abs(measures[mi][1]-length)>1e-7:
                raise ValueError('Parts must share measure boundaries')
            start += length
        if ties: raise ValueError('Unterminated MusicXML tie')
        score_end = max(score_end,start)
    return notes,measures,score_end


def putvlq(n):
    out = [n & 127]; n >>= 7
    while n: out.insert(0,(n & 127)|128); n >>= 7
    return bytes(out)


def track(events):
    result = bytearray(); before = 0
    for tick,priority,data in sorted(events,key=lambda e:(e[0],e[1])):
        result.extend(putvlq(tick-before)); result.extend(data); before = tick
    result.extend(b'\x00\xff\x2f\x00')
    return b'MTrk'+struct.pack('>I',len(result))+result


def midi_write(path,notes,labels,num,den,tempo):
    ppq = 9600
    if den & (den-1): raise ValueError('MIDI time signature denominator must be a power of two')
    micro = round(60000000/tempo)
    events = [(0,0,b'\xff\x51\x03'+micro.to_bytes(3,'big')),
              (0,0,b'\xff\x58\x04'+bytes([num,int(math.log2(den)),24,8]))]
    for label in labels:
        text = label['text'].encode('utf-8')
        events.append((round(float(label['onset'])*ppq),0,b'\xff\x06'+putvlq(len(text))+text))
    tracks = [track(events)]
    for voice in sorted({n['voice'] for n in notes}):
        events = []; channel = voice-1
        for n in notes:
            if n['voice']!=voice: continue
            start = round(n['onset']*ppq); end = round((n['onset']+n['duration'])*ppq)
            if end<=start: raise ValueError('Duration below MIDI resolution')
            events.extend([(start,1,bytes([0x90+channel,n['pitch'],64])),(end,0,bytes([0x80+channel,n['pitch'],0]))])
        tracks.append(track(events))
    path.write_bytes(b'MThd'+struct.pack('>IHHH',6,1,len(tracks),ppq)+b''.join(tracks))


def sub(parent,tag,text=None,**attrs):
    e = ET.SubElement(parent,tag,{k:str(v) for k,v in attrs.items()})
    if text is not None: e.text = str(text)
    return e


def xml_write(path,notes,labels,num,den,tempo):
    # Use an exact shared divisions value for representable rational durations.
    div = 1
    for n in notes:
        for field in ('onset','duration'):
            f = Fraction(n[field]).limit_denominator(9600)
            if abs(float(f)-n[field])>1e-7: raise ValueError('Time not representable at supported MusicXML resolution')
            div = lcm(div,f.denominator)
    div = lcm(div,den)
    if div>1000000: raise ValueError('MusicXML divisions would be excessive')
    bar = num*4/den; end = max(n['onset']+n['duration'] for n in notes)
    count = math.ceil((end-1e-8)/bar)
    root = ET.Element('score-partwise',version='4.0'); pl = sub(root,'part-list')
    for voice in sorted({n['voice'] for n in notes}):
        sp = sub(pl,'score-part',id=f'P{voice}'); sub(sp,'part-name',f'Aani {voice}')
    for voice in sorted({n['voice'] for n in notes}):
        part = sub(root,'part',id=f'P{voice}')
        for mi in range(count):
            a,b = mi*bar,(mi+1)*bar
            measure = sub(part,'measure',number=mi+1)
            if mi==0:
                attrs = sub(measure,'attributes'); sub(attrs,'divisions',div)
                time = sub(attrs,'time'); sub(time,'beats',num); sub(time,'beat-type',den)
                clef = sub(attrs,'clef'); sub(clef,'sign','G' if voice<=2 else 'F'); sub(clef,'line',2 if voice<=2 else 4)
                direction = sub(measure,'direction'); dt = sub(direction,'direction-type'); met = sub(dt,'metronome')
                sub(met,'beat-unit','quarter'); sub(met,'per-minute',tempo); sub(direction,'sound',tempo=str(tempo))
            if voice==1:
                for label in labels:
                    if a-1e-8<=float(label['onset'])<b-1e-8:
                        # Rehearsal marks survive notation-program import as structural labels.
                        marker = sub(measure,'direction',placement='above')
                        dt = sub(marker,'direction-type')
                        sub(dt,'rehearsal',label['text'].split(';',1)[0],**{'default-y':'70'})
                        sub(marker,'offset',round((float(label['onset'])-a)*div))
                        direction = sub(measure,'direction',placement='above')
                        dt = sub(direction,'direction-type')
                        sub(dt,'words',label['text'],**{'font-size':'9','default-y':'35'})
                        sub(direction,'offset',round((float(label['onset'])-a)*div))
            selected = sorted((n for n in notes if n['voice']==voice and n['onset']<b-1e-8 and n['onset']+n['duration']>a+1e-8),key=lambda n:n['onset'])
            cursor = a
            def emit(start,finish,n=None):
                note = sub(measure,'note')
                if n is None: sub(note,'rest')
                else:
                    p = sub(note,'pitch'); pc = n['pitch']%12
                    step,alter = [('C',0),('C',1),('D',0),('E',-1),('E',0),('F',0),('F',1),('G',0),('A',-1),('A',0),('B',-1),('B',0)][pc]
                    sub(p,'step',step)
                    if alter: sub(p,'alter',alter)
                    sub(p,'octave',n['pitch']//12-1)
                sub(note,'duration',round((finish-start)*div))
                ties = []
                if n is not None:
                    if n['onset']<start-1e-8: ties.append('stop')
                    if n['onset']+n['duration']>finish+1e-8: ties.append('start')
                    for t in ties: sub(note,'tie',type=t)
                sub(note,'voice',1)
                if ties:
                    notation = sub(note,'notations')
                    for t in ties: sub(notation,'tied',type=t)
            for n in selected:
                start,finish = max(a,n['onset']),min(b,n['onset']+n['duration'])
                if start>cursor+1e-8: emit(cursor,start)
                emit(start,finish,n); cursor = finish
            if cursor<b-1e-8: emit(cursor,b)
    indent_xml(root)
    ET.ElementTree(root).write(path,encoding='utf-8',xml_declaration=True)


def main():
    mode,path,folder = sys.argv[1:]; path,folder = Path(path),Path(folder)
    ext = path.suffix.lower()
    if mode=='read':
        parsed = midi_read(path) if ext in ('.mid','.midi') else xml_read(path) if ext in ('.xml','.musicxml','.mxl') else None
        if parsed is None: raise ValueError('Use .mid, .midi, .xml, .musicxml or .mxl')
        save_input(folder,*parsed)
    elif mode=='write':
        notes = table(folder/'notes.csv'); labels = table(folder/'labels.csv')
        for n in notes:
            for k in ('onset','duration'): n[k] = float(n[k])
            for k in ('pitch','voice'): n[k] = int(float(n[k]))
        num,den,tempo = map(float,(folder/'settings.txt').read_text().split()); num,den = int(num),int(den)
        if ext in ('.mid','.midi'): midi_write(path,notes,labels,num,den,tempo)
        elif ext in ('.xml','.musicxml'): xml_write(path,notes,labels,num,den,tempo)
        else: raise ValueError('Output must be .mid or .musicxml')
    else: raise ValueError('Unknown mode')

if __name__=='__main__':
    try: main()
    except Exception as exc:
        print(f'Notation IO: {exc}',file=sys.stderr); sys.exit(1)
