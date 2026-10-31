"""Reproduce pre-3.9 Python: math.lcm and ET.indent are absent."""
import importlib.util
from pathlib import Path
import tempfile
import math
import xml.etree.ElementTree as ET

spec = importlib.util.spec_from_file_location('notation_io',Path(__file__).resolve().parents[1]/'tools'/'notation_io.py')
io = importlib.util.module_from_spec(spec)
spec.loader.exec_module(io)
old_lcm = getattr(math,'lcm',None)
old_indent = getattr(ET,'indent',None)
if old_lcm is not None: del math.lcm
if old_indent is not None: del ET.indent
try:
    with tempfile.TemporaryDirectory() as folder:
        path = Path(folder)/'compat.musicxml'
        notes = [dict(onset=0.,duration=6.,pitch=60,voice=1),
                 dict(onset=1/3,duration=1/3,pitch=64,voice=2),
                 dict(onset=2/3,duration=1/3,pitch=67,voice=2)]
        labels = [dict(onset=0,text='Ratkaisu 1; muutokset 0/0; virheet 0; 1/T1/O/1/0/0'),
                  dict(onset=4,text='Ratkaisu 2; muutokset 1/2; virheet 3; 2/T1/I/0.5/4/-12')]
        io.xml_write(path,notes,labels,4,4,60)
        root = ET.parse(path).getroot()
        assert root.tag == 'score-partwise'
        part = root.find('part')
        assert [e.text for e in part.findall('.//rehearsal')] == ['Ratkaisu 1','Ratkaisu 2']
        assert [e.text for e in part.findall('.//words')] == [label['text'] for label in labels]
        assert part.findall('measure')[1].find('.//rehearsal').text == 'Ratkaisu 2'
        assert not root.findall('part')[1].findall('.//words')
        restored,_,_ = io.xml_read(path)
        assert len(restored)==3
        assert any(abs(n[1]-6)<1e-8 for n in restored)
        assert any(abs(n[0]-1/3)<1e-8 for n in restored)
        assert '<html' not in path.read_text()
finally:
    if old_lcm is not None: math.lcm = old_lcm
    if old_indent is not None: ET.indent = old_indent
print('MusicXML export without math.lcm / ET.indent OK')
