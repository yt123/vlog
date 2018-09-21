from glob import glob
import csv
import codecs
import chardet
import os.path

def filename_without_extension(filename):
    filename = os.path.basename(filename)
    place = filename.find('.')
    if place < 0:
        return filename
    return filename[:place]

def detect_encoding(filename):
    with open(filename, 'rb') as f:
        result = chardet.detect(f.read())
        return result['encoding']

files = glob("transcripts/*.txt")

new = []
for file in files:
    encoding = detect_encoding(file)
    with codecs.open(file, encoding=encoding) as f:
        content = f.read()
        content = content.replace('\n', '')
        new.append([filename_without_extension(file), content])

with codecs.open('transcripts.csv', 'w', encoding='utf-8') as csvf:
    writer = csv.writer(csvf, delimiter=';')
    writer.writerows(new)
