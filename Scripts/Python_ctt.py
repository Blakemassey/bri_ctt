import csv  #library (should be pre-installed)
import os   #library (should be pre-installed)
import mechanize, datetime # library (may need to import)

br = mechanize.Browser()
br.set_handle_robots(False)   # ignore robots
br.set_handle_refresh(False)  # can sometimes hang without this
br.addheaders = [('User-agent', 'Firefox')]
today = datetime.date.today().strftime("%Y-%m-%d 23:59:59")
week_ago = datetime.date.today() - datetime.timedelta(days=7) # goes back a week
week_ago = week_ago.strftime("%Y-%m-%d %H:%M:%S")

response = br.open("https://account.celltracktech.com/accounts/login")
br.form = list(br.forms())[0]
br.form["username"] = "USERNAME"
br.form["password"] = "PASSWORD"
response = br.submit()

br.open('https://account.celltracktech.com/data/download/1787/') #Sprague
fileobj = open('C:/Work/R/Projects/bri_ctt/Data/CTT/1D658.csv', "w+")
br.select_form(name="dataExportForm")
br.form = list(br.forms())[0]
br.form["startDt"] = week_ago; br.form["endDt"] = today
response = br.submit(); data = response.read()
fileobj.write(data); fileobj.close()

br.open('https://account.celltracktech.com/data/download/1788/') #Machias
fileobj = open('C:/Work/R/Projects/bri_ctt/Data/CTT/1D6F4.csv', "w+")
br.select_form(name="dataExportForm")
br.form = list(br.forms())[0]
br.form["startDt"] = week_ago; br.form["endDt"] = today
response = br.submit(); data = response.read()
fileobj.write(data); fileobj.close()

br.open('https://account.celltracktech.com/data/download/1796/') #Wash
fileobj = open('C:/Work/R/Projects/bri_ctt/Data/CTT/1DFAB.csv', "w+")
br.select_form(name="dataExportForm")
br.form = list(br.forms())[0]
br.form["startDt"] = week_ago; br.form["endDt"] = today
response = br.submit(); data = response.read()
fileobj.write(data); fileobj.close()

br.open('https://account.celltracktech.com/data/download/1758/') #Wash
fileobj = open('C:/Work/R/Projects/bri_ctt/Data/CTT/1B570.csv', "w+")
br.select_form(name="dataExportForm")
br.form = list(br.forms())[0]
br.form["startDt"] = week_ago; br.form["endDt"] = today
response = br.submit(); data = response.read()
fileobj.write(data); fileobj.close()
