library(httr)

# 1. Find OAuth settings for github:
#    http://developer.github.com/v3/oauth/
oauth_endpoints("github")

# 2. Register an application at https://github.com/settings/applications;
#    Use any URL you would like for the homepage URL (http://github.com is fine)
#    and http://localhost:1410 as the callback url
#
#    Insert your client ID and secret below - if secret is omitted, it will
#    look it up in the GITHUB_CONSUMER_SECRET environmental variable.
myapp <- oauth_app(appname = "github", key = "05b38d4da172aa26cce5", secret = "5e553e0a254c9553cb9b91b54b7c44d65275a84d")

# 3. Get OAuth credentials
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

# 4. Use API
gtoken <- config(token = github_token)
#req <- GET("https://api.github.com/rate_limit", gtoken)
req <- GET("https://api.github.com/users/jtleek/repos")
stop_for_status(req)
content(req)

library(jsonlite)
jsoned <- fromJSON(toJSON(content(req)))

View(jsoned)
## will display in matrix form

jsoned[jsoned$name == "datasharing",]$created_at
## get the time that the datasharing repo was craeted

# OR:
##req <- with_config(gtoken, GET("https://api.github.com/rate_limit"))
##stop_for_status(req)
##content(req)
