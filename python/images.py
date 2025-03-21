from bs4 import BeautifulSoup
from selenium import webdriver
from polars import DataFrame, col

leagues = ['mens', 'womens']

for league in leagues:

    # get post-load html via selenium
    url = f'https://www.espn.com/{league}-college-basketball/teams'

    browser = webdriver.Chrome()
    browser.get(url)
    soup = BeautifulSoup(browser.page_source, 'html.parser')
    browser.close()

    # container for image links
    images = []

    # add image links for each team
    teams = soup.find_all(class_='ContentList__Item')
    for team in teams:
        link = team.find(class_='Logo').attrs.get('src')
        images.append(link)

    (
        DataFrame({'image': images})
        .select(col('image').str.slice(offset=0, length=col('image').str.find('png') + 3))
        .with_columns(col('image').str.slice(offset=col('image').str.find('500/') + 4).alias('team_id'))
        .with_columns(col('team_id').str.replace('_ncw', ''))
        .select(col('image'),
                col('team_id').str.replace('.png', ''))
        .write_parquet(f'data/images/{league}-images.parquet')
    )