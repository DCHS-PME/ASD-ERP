# Scrape 211 reports by zip code
# Max Griswold
# 6/29/26

import os
import csv
import random
import asyncio
import urllib.parse
from datetime import datetime
from bs4 import BeautifulSoup
from playwright.async_api import async_playwright
 
print("hello")

KC_ZIPS = {
    "98001","98002","98003","98004","98005","98006","98007","98008","98009",
    "98010","98011","98013","98014","98015","98019","98022","98023","98024",
    "98025","98027","98028","98029","98030","98031","98032","98033","98034",
    "98035","98038","98039","98040","98041","98042","98045","98047","98050",
    "98051","98052","98053","98055","98056","98057","98058","98059","98062",
    "98064","98065","98068","98070","98071","98072","98073","98074","98075",
    "98077","98083","98089","98092","98093","98101","98102","98103","98104",
    "98105","98106","98107","98108","98109","98111","98112","98113","98114",
    "98115","98116","98117","98118","98119","98121","98122","98124","98125",
    "98126","98127","98129","98131","98132","98133","98134","98136","98138",
    "98139","98141","98144","98145","98146","98148","98154","98155","98158",
    "98160","98161","98164","98165","98166","98168","98170","98174","98175",
    "98177","98178","98181","98185","98188","98190","98191","98194","98195",
    "98198","98199","98224","98288", "98354", "98422"
}
 
zip_to_id_map = {}
with open('./data/wa211_zip_id_cw.csv', mode='r', newline='', encoding='utf-8') as file:
    for row in csv.DictReader(file):
        if row['zip'] in KC_ZIPS:
            zip_to_id_map[row['zip']] = row['id']
print(f"Loaded {len(zip_to_id_map)} King County zips.")
 
MAX_RETRIES = 3
COOLDOWN_SECONDS = 300
POLITE_MIN, POLITE_MAX = 4, 9
 
CAT_FIELDS = ["zip", "year", "category_id", "category", "count", "percentage", "total_requests"]
SUB_FIELDS = ["zip", "year", "parent_category_id", "parent_category",
              "subcategory_id", "subcategory", "count", "percentage",
              "unmet_count", "unmet_percentage"]
 
 
def parse_fragment(html, zip_code, year):
    """Return (category_rows, subcategory_rows) from one barChart HTML fragment."""
    soup = BeautifulSoup(html, "html.parser")
 
    total = None
    t = soup.select_one("div.categoriesDiv span.value.total-value[data-value]")
    if t:
        total = t["data-value"].replace(",", "")
 
    # --- top-level categories ---
    cat_rows, id_to_name = [], {}
    for div in soup.select("div.categoriesDiv div.categories"):
        val = div.select_one("span.value[data-value]")
        lbl = div.select_one("span.toolTipSubCategory")
        if not (val and lbl):          # skips Total row + legend blocks
            continue
        cid = div.get("data-id")
        id_to_name[cid] = lbl.get_text(strip=True)
        cat_rows.append({
            "zip": zip_code, "year": year, "category_id": cid,
            "category": lbl.get_text(strip=True),
            "count": val["data-value"].replace(",", ""),
            "percentage": val.get("data-percentage", ""),
            "total_requests": total,
        })
 
    # --- subcategories (with unmet-need counts) ---
    sub_rows = []
    for ul in soup.select("div.subcategoriesDiv ul.list.details"):
        pid = ul.get("id", "").replace("subcategory-", "")
        for li in ul.select("li"):
            left = li.select_one("div.leftDivOfUnmetNeeds span.value[data-value]")
            lbl = li.select_one("div.leftDivOfUnmetNeeds span.toolTipSubCategory")
            if not (left and lbl):     # skips dummy rows, legends, "Not Available"
                continue
            unmet = li.select_one("div.rightDivOfUnmetNeeds span.value[data-value]")
            idd = left.find_parent(attrs={"data-id": True})
            sub_rows.append({
                "zip": zip_code, "year": year,
                "parent_category_id": pid,
                "parent_category": id_to_name.get(pid, ""),
                "subcategory_id": idd.get("data-id") if idd else "",
                "subcategory": lbl.get_text(strip=True),
                "count": left["data-value"].replace(",", ""),
                "percentage": left.get("data-percentage", ""),
                "unmet_count": unmet["data-value"] if unmet else "",
                "unmet_percentage": unmet.get("data-percentage", "") if unmet else "",
            })
    return cat_rows, sub_rows
 
 
def open_appender(path, fields):
    new = not os.path.exists(path) or os.path.getsize(path) == 0
    f = open(path, "a", newline="", encoding="utf-8")
    w = csv.DictWriter(f, fieldnames=fields)
    if new:
        w.writeheader()
        f.flush()
    return f, w
 
 
async def scrape_211(start_date_str, end_date_str):
    start_dt = datetime.strptime(start_date_str, "%m/%d/%y")
    end_dt = datetime.strptime(end_date_str, "%m/%d/%y")
    year = start_dt.year
 
    os.makedirs("./data", exist_ok=True)
    stamp = f"{start_dt:%Y-%m-%d}_to_{end_dt:%Y-%m-%d}"
    cat_csv = f"./data/wa211_categories_{stamp}.csv"
    sub_csv = f"./data/wa211_subcategories_{stamp}.csv"
 
    from_date = f"{start_dt.strftime('%b')} {start_dt.day}, {start_dt.strftime('%Y')}"
    to_date = f"{end_dt.strftime('%b')} {end_dt.day}, {end_dt.strftime('%Y')}"
    from_date_encoded = urllib.parse.quote_plus(from_date)
    to_date_encoded = urllib.parse.quote_plus(to_date)
 
    # resume off the CATEGORY file (written last, so its presence => sub also written)
    done = set()
    if os.path.exists(cat_csv):
        with open(cat_csv, newline="", encoding="utf-8") as f:
            done = {r["zip"] for r in csv.DictReader(f)}
        print(f"Resuming: {len(done)} zips already done")
 
    sub_file, sub_writer = open_appender(sub_csv, SUB_FIELDS)
    cat_file, cat_writer = open_appender(cat_csv, CAT_FIELDS)
 
    async with async_playwright() as p:
        browser = await p.chromium.launch(
            headless=False,
            executable_path=r"C:\Users\mgriswol\AppData\Local\Google\Chrome\Application\chrome.exe",
            args=["--disable-blink-features=AutomationControlled"],
        )
        context = await browser.new_context()
        page = await context.new_page()
        await page.goto("https://wa.211counts.org/", wait_until="commit")
        await asyncio.sleep(5)
 
        for zip_code, internal_id in zip_to_id_map.items():
            if zip_code in done:
                print(f"Skip {zip_code} (already done)")
                continue
 
            payload_str = (
                f"identifierCategory=&sourceType=&fromMobile=false"
                f"&id=%7B%22ids%22%3A%5B%22{internal_id}%22%5D%7D"
                f"&timeIntervalId=0&centerId=62"
                f"&fromDate={from_date_encoded}&toDate={to_date_encoded}"
                f"&type=Z&methodOfContacts=%7B%7D"
            )
            js_script = f"""
            async () => {{
                const r = await fetch("https://wa.211counts.org/dashBoard/barChart", {{
                    method: "POST",
                    headers: {{ "Content-Type": "application/x-www-form-urlencoded" }},
                    body: "{payload_str}"
                }});
                if (!r.ok) throw new Error("HTTP " + r.status);
                return await r.text();
            }}
            """
 
            for attempt in range(1, MAX_RETRIES + 1):
                try:
                    html = await page.evaluate(js_script)
                    cats, subs = parse_fragment(html, zip_code, year)
                    if not cats:
                        print(f"WARNING {zip_code}: no categories (empty/blocked) -- not writing")
                        break
                    # write subs FIRST, then cats: cat presence is the resume marker
                    if subs:
                        sub_writer.writerows(subs)
                        sub_file.flush()
                    cat_writer.writerows(cats)
                    cat_file.flush()
                    print(f"Saved {zip_code}: {len(cats)} categories, {len(subs)} subcategories")
                    break
                except Exception as e:
                    if "403" in str(e) and attempt < MAX_RETRIES:
                        print(f"403 at {zip_code} ({attempt}/{MAX_RETRIES}) -- cooling {COOLDOWN_SECONDS//60} min")
                        await asyncio.sleep(COOLDOWN_SECONDS)
                        continue
                    print(f"Failed {zip_code}: {e}")
                    break
 
            await asyncio.sleep(random.uniform(POLITE_MIN, POLITE_MAX))
 
        await browser.close()
 
    sub_file.close()
    cat_file.close()
    print(f"Done -> {cat_csv}\n     -> {sub_csv}")
 
asyncio.run(scrape_211("1/1/25", "12/31/25"))