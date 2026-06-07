import re
import pandas as pd

try:
    with open("Konstytucja.txt", "r", encoding="utf-8") as file:
        text = file.read()
except FileNotFoundError:
    print("Błąd: Nie znaleziono pliku 'konstytucja.txt' w folderze projektu!")
    exit()

text = re.sub(r'©\s*Kancelaria\s+Sejmu', '', text)


articles_raw = re.split(r'(?=Art\.\s+\d+\.)', text)

preambula = articles_raw[0].strip()
articles_list = articles_raw[1:]

parsed_articles = []

if preambula:
    preambula_clean = re.sub(r'\s+', ' ', preambula).strip()
    parsed_articles.append({
        "id": "Preambuła",
        "text": preambula_clean
    })

for art in articles_list:
    art = art.strip()
    if not art:
        continue

    match = re.match(r'(Art\.\s+\d+\.)', art)
    if match:
        art_id = match.group(1)
        art_content = art[len(art_id):].strip()

        art_content_clean = re.sub(r'\s+', ' ', art_content).strip()

        parsed_articles.append({
            "id": art_id,
            "text": art_content_clean
        })

df = pd.DataFrame(parsed_articles)

df.to_csv("konstytucja_artykuly.csv", index=False, encoding="utf-8")

print(f"Sukces! Przetworzono tekst i utworzono plik 'konstytucja_artykuly.csv'.")
print(f"Liczba dokumentów (Preambuła + Artykuły): {len(df)}")