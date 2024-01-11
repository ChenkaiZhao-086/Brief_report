import os
import pandas as pd
import datetime as dtm
import requests
from selenium import webdriver
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.common.action_chains import ActionChains
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.by import By # 按照什么方法查找，ID、CSS等
from selenium.webdriver.support import expected_conditions as EC
import time
import shutil

u_date = dtm.datetime.today().strftime("%Y-%m-%d")
os.mkdir(f"{u_date}")
loca = ['Hong Kong', 'Singapore', 'Japan', 'South Korea', 'United Kingdom', 'Australia', 'United States', 'South Africa']



### Download raw data
case_url = "https://covid.ourworldindata.org/data/owid-covid-data.csv"
with open(f"{u_date}/{u_date}.csv", "wb") as f:
        f.write(requests.get(case_url, headers={'user-agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) \
           AppleWebKit/537.36 (KHTML, like Gecko) \
           Chrome/112.0.0.0 Safari/537.36'}).content)

### Data clean     
dat = pd.read_csv(f"{u_date}/{u_date}.csv")
col_nam = ['iso_code', 'continent', 'location', 'date', 'new_cases', 'new_deaths']

# 检查是哪一天开始报告，如果不是周一则找到周一对应的时间间隔
day_interval = 365
while (dtm.datetime.today() - dtm.timedelta(days=day_interval)).weekday() != 0:
        day_interval += 1

dat2 = dat.loc[dat['location'].isin(loca) & (dat['date'] >= (dtm.datetime.today() - dtm.timedelta(days=day_interval)).strftime('%Y-%m-%d')), col_nam]
dat2['wek'] = pd.to_datetime(dat2['date']).dt.isocalendar().week
# 汇总
dat2 = (dat2.groupby(['location', 'wek']).agg({'iso_code': 'first', 'continent': 'first', 'date': 'first', 'new_cases': 'sum', 'new_deaths': 'sum'})
            .sort_values(by=['continent', 'location', 'date'])
            .reset_index()
            .rename(columns={'new_cases': 'case', 'new_deaths': 'death'}))
# dat2 = dat2.drop_duplicates(subset=['location', 'wek'], keep='first')
dat2['date'] = pd.to_datetime(dat2['date'])
dat2['lable'] = dat2['date'].dt.strftime('%Y-W%U')
dat2.to_csv(f"{u_date}/{u_date}_clean.csv", index=False)



def all(url = 'https://ourworldindata.org/covid-cases#coronavirus-sequences-by-variant', move_path = '/Users/faust/Documents/600_Project/Brief_report'):
    ### All distribution
    browser = webdriver.Chrome()
    browser.set_window_size(1080, 1080) # 设定窗口大小，避免不同的大小运行出错
    browser.get(url)
    browser.execute_script("window.scrollBy(0, 20);")
    browser.implicitly_wait(3)
    time.sleep(5)

    # rejective cookies
    CookiesCSS = "body > div:nth-child(9) > div > div > div > div.actions > button:nth-child(1)"
    browser.find_element(By.CSS_SELECTOR, CookiesCSS).click()
    # wait.until(EC.presence_of_element_located((By.CSS_SELECTOR, CookiesCSS))).click()


    wait = WebDriverWait(browser, 10)
    # 右侧滑块及时间
    Time2CSS = "body > main > article > div.content-wrapper > div.offset-content > div > div > section:nth-child(6) > div.wp-block-columns.is-style-side-by-side > div:nth-child(2) > figure:nth-child(1) > div > div.TimelineComponent > div:nth-child(4)"
    time2 = wait.until(EC.presence_of_element_located((By.CSS_SELECTOR, Time2CSS))).text# get_attribute("textContent")
    # 网站的结构发生了变化，注释掉的是之前的结构，下面的滑块和while循环一样
    # body > main > article > div.content-wrapper > div.offset-content > div > div > section:nth-child(6) > div.wp-block-columns.is-style-side-by-side > div:nth-child(2) > figure:nth-child(1) > div > div:nth-child(3) > svg > g > g.DualAxisView > g.HorizontalAxis > text:nth-child(4)
    time2 = dtm.datetime.strptime(time2, "%b %d, %Y")
    #time.sleep(2)
    # 滑动滑块
    sliderCSS = "body > main > article > div.content-wrapper > div.offset-content > div > div > section:nth-child(6) > div.wp-block-columns.is-style-side-by-side > div:nth-child(2) > figure:nth-child(1) > div > div.TimelineComponent > div.slider.clickable > div.handle.startMarker > div"
    slider = browser.find_element(By.CSS_SELECTOR, sliderCSS)
    # body > main > article > div.content-wrapper > div.offset-content > div > div > section:nth-child(6) > div.wp-block-columns.is-style-side-by-side > div:nth-child(2) > figure:nth-child(1) > div > div.ControlsFooter > div:nth-child(1) > div > div.slider.clickable > div.handle.startMarker
    action = ActionChains(browser)
    action.drag_and_drop_by_offset(slider, 100, 0).perform() # 先做一个快速的粗调节
    time.sleep(1)

    Time1CSS = "body > main > article > div.content-wrapper > div.offset-content > div > div > section:nth-child(6) > div.wp-block-columns.is-style-side-by-side > div:nth-child(2) > figure:nth-child(1) > div > div:nth-child(5) > svg > g > g.DualAxisView > g.HorizontalAxis > text:nth-child(3)"
    while True:  
        # 左侧滑块及时间
        #time.sleep(0.5)
        time1 = wait.until(EC.presence_of_element_located((By.CSS_SELECTOR, Time1CSS))).text # get_attribute("textContent")
        time1 = dtm.datetime.strptime(time1, "%b %d, %Y")
        days = (time2.date() - time1.date()).days
        if days >= (365-15) and days <= (365+15):
            break
        # 滑动滑块
        action.drag_and_drop_by_offset(slider, 4, 0).perform()
        browser.execute_script("window.scrollBy(0, 20);")

    time.sleep(5)
    # 点下载
    DownloadCSS_1a = "body > main > article > div.content-wrapper > div.offset-content > div > div > section:nth-child(6) > div.wp-block-columns.is-style-side-by-side > div:nth-child(2) > figure:nth-child(1) > div > footer > div.SourcesFooterHTMLBottom > div.ActionButtons > ul > li:nth-child(1) > div > button"
    browser.execute_script("window.scrollBy(0, 150);") # 如果不滚动一下屏幕，会导致无法点击到下载键
    # browser.find_element(By.CSS_SELECTOR, DownloadCSS_1a).click()
    wait.until(EC.presence_of_element_located((By.CSS_SELECTOR, DownloadCSS_1a))).click()
    time.sleep(1)
    # 下载图
    DownloadCSS_1b = "body > main > article > div.content-wrapper > div.offset-content > div > div > section:nth-child(6) > div.wp-block-columns.is-style-side-by-side > div:nth-child(2) > figure:nth-child(1) > div > div.modalOverlay > div > div > div.modalScrollable > div > div > div:nth-child(1) > div > button:nth-child(1) > div:nth-child(3) > span"
    browser.find_element(By.CSS_SELECTOR, DownloadCSS_1b).click()
    # wait.until(EC.presence_of_element_located((By.CSS_SELECTOR, DownloadCSS_1b))).click()
    time.sleep(1)
    # 下载数据
    DownloadCSS_1c = "body > main > article > div.content-wrapper > div.offset-content > div > div > section:nth-child(6) > div.wp-block-columns.is-style-side-by-side > div:nth-child(2) > figure:nth-child(1) > div > div.modalOverlay > div > div > div.modalScrollable > div > div > div.grouped-menu-section.grouped-menu-section-data > div > button > div.grouped-menu-icon > span"
    browser.find_element(By.CSS_SELECTOR, DownloadCSS_1c).click()
    # wait.until(EC.presence_of_element_located((By.CSS_SELECTOR, DownloadCSS_1c))).click()
    time.sleep(3)

    #browser.refresh()

    ### VOC distribution
    browser.execute_script("window.scrollBy(0, 850);")
    browser.implicitly_wait(3)
    # # 点Relative
    # wait.until(EC.presence_of_element_located((By.CSS_SELECTOR, 'body > main > article > div.content-wrapper > div.offset-content > div > div > section:nth-child(6) > div.wp-block-columns.is-style-side-by-side > div:nth-child(1) > figure > div > div.controlsRow > div > ul > li:nth-child(2) > label > input[type=checkbox]'))).click()
    # 调节滑块
    sliderCSS2 = "body > main > article > div.content-wrapper > div.offset-content > div > div > section:nth-child(6) > div.wp-block-columns.is-style-side-by-side > div:nth-child(1) > figure > div > div.TimelineComponent > div.slider.clickable > div.handle.endMarker"
    slider = browser.find_element(By.CSS_SELECTOR, sliderCSS2)
    action = ActionChains(browser)
    action.drag_and_drop_by_offset(slider, 100, 0).perform()
    # time.sleep(0.5)
    # 点 add country
    EditCSS = "body > main > article > div.content-wrapper > div.offset-content > div > div > section:nth-child(6) > div.wp-block-columns.is-style-side-by-side > div:nth-child(1) > figure > div > nav > div.chart-controls > div.entity-selection-menu > button > label"
    # wait.until(EC.element_to_be_clickable((By.CSS_SELECTOR, EditCSS))).click()
    browser.find_element(By.CSS_SELECTOR, EditCSS).click()

    # 点清除
    ClearCSS = "body > main > article > div.content-wrapper > div.offset-content > div > div > section:nth-child(6) > div.wp-block-columns.is-style-side-by-side > div:nth-child(1) > figure > div > div.modalOverlay > div > div > div.modalScrollable > div > div.searchBar > button"
    browser.find_element(By.CSS_SELECTOR, ClearCSS).click()
    # wait.until(EC.presence_of_element_located((By.CSS_SELECTOR, "body > main > article > div.content-wrapper > div.offset-content > div > div > section:nth-child(6) > div.wp-block-columns.is-style-side-by-side > div:nth-child(1) > figure > div > div.entitySelectorOverlay > div > div > div.selectedData > button > span > svg"))).click()
    # time.sleep(1)
    
    # 输入
    SearchBoxCSS = "body > main > article > div.content-wrapper > div.offset-content > div > div > section:nth-child(6) > div.wp-block-columns.is-style-side-by-side > div:nth-child(1) > figure > div > div.modalOverlay > div > div > div.modalScrollable > div > div.searchBar > div > input[type=search]"
    search_box = browser.find_element(By.CSS_SELECTOR, SearchBoxCSS)
    for i in loca:
        search_box.send_keys(i)
        search_box.send_keys(Keys.ENTER)
    # 点关闭
    search_box.send_keys(Keys.ESCAPE)
    time.sleep(1)
    # 点下载
    DownloadCSS_2a = "body > main > article > div.content-wrapper > div.offset-content > div > div > section:nth-child(6) > div.wp-block-columns.is-style-side-by-side > div:nth-child(1) > figure > div > footer > div.SourcesFooterHTMLBottom > div.ActionButtons > ul > li:nth-child(1) > div > button"
    browser.execute_script("window.scrollBy(0, 150);")
    browser.find_element(By.CSS_SELECTOR, DownloadCSS_2a).click()
    time.sleep(2)
    # wait.until(EC.presence_of_element_located((By.CSS_SELECTOR, DownloadCSS_2a))).click()
    # 下载
    DownloadCSS_2b = "body > main > article > div.content-wrapper > div.offset-content > div > div > section:nth-child(6) > div.wp-block-columns.is-style-side-by-side > div:nth-child(1) > figure > div > div.modalOverlay > div > div > div.modalScrollable > div > div > div:nth-child(1) > div > button:nth-child(1)"
    browser.find_element(By.CSS_SELECTOR, DownloadCSS_2b).click()
    # wait.until(EC.presence_of_element_located((By.CSS_SELECTOR, DownloadCSS_2b))).click()
    time.sleep(2)

    DownloadCSS_2c ="body > main > article > div.content-wrapper > div.offset-content > div > div > section:nth-child(6) > div.wp-block-columns.is-style-side-by-side > div:nth-child(1) > figure > div > div.modalOverlay > div > div > div.modalScrollable > div > div > div.grouped-menu-section.grouped-menu-section-data > div > button"
    browser.find_element(By.CSS_SELECTOR, DownloadCSS_2c).click()
    # wait.until(EC.presence_of_element_located((By.CSS_SELECTOR, "body > main > article > div.content-wrapper > div.offset-content > div > div > section:nth-child(6) > div.wp-block-columns.is-style-side-by-side > div:nth-child(1) > figure > div > div.DownloadTab > div > div:nth-child(1) > div > button:nth-child(1)"))).click()

    time.sleep(4)
    # if os.path.exists('/Users/faust/Downloads/covid-variants-area.png') and os.path.exists('/Users/faust/Downloads/covid-variants-bar.png'):
    #     shutil.move('/Users/faust/Downloads/covid-variants-area.png', move_path)
    #     shutil.move('/Users/faust/Downloads/covid-variants-area.csv', move_path)
    #     shutil.move('/Users/faust/Downloads/covid-variants-bar.png', move_path)
    #     shutil.move('/Users/faust/Downloads/covid-variants-bar.csv', move_path)

    #     print('Done')
    # else:
    #     print('Warning!!! Check png')

    shutil.move('/Users/faust/Downloads/covid-variants-area.png', move_path)
    shutil.move('/Users/faust/Downloads/covid-variants-area.csv', move_path)
    shutil.move('/Users/faust/Downloads/covid-variants-bar.png', move_path)
    shutil.move('/Users/faust/Downloads/covid-variants-bar.csv', move_path)
    
    browser.quit()


all(move_path = f'/Users/faust/Documents/600_Project/Brief_report/{u_date}')


