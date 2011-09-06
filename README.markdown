## Erlang Github API

Wrapper for github.com API written in Erlang.

## Status
The erlang-github-api project in very very early stage, it's only started to be developed.

## Implemented

  * github.erl - general github api.  -> progress  - 100%
  * auth.erl - authorization api. -> progrss - 50% (implemented basic auth)
  * blobs.erl - blob api -> progress - 100% (implemeted get blob content, encoding, create new blob)
  * define.hrl - github api urls
  * gist.erl - github gist api -> progress 90% (implemented star, unstar, createe new gist, delete, get author, get content ...)
  * messages.erl - make message to json format
  * user.erl - github user api -> progress 90% (implmenented get user, get user data: email, login, bio, location and etc ...)
  * utils.erl - utils functions

## Features

  * Support Gists
  * Git api
  * Support Issues 
  * Organizations
  * Repos
  * Users
  * Full support github API v.3
  
## Build
```
make
```
  
## Usage
Example for usage, Get gist description:

```erlang
%
% get gist description
%
1> gist:get_gist_description(1).

["the meaning of gist"]

%
% get gist pull url
%
2> gist:get_gist_pull_url(1).

["git://gist.github.com/1.git"]

%
% get gist public or private
%
3> gist:is_public(1).

true

%
% create new gist
%
4> gist:create_gist("github_user_name", "password", "Description", true, "file.txt", "Gist content").

>>> ok

```
  
## Dependency

  * Ibrowse (https://github.com/cmullaparthi/ibrowse)
  * Thank you lambder for json.er and json.hrl (https://github.com/lambder/jsonerl)
  
## erlang-github-api TODO and Issues
Issues - <https://github.com/onlyshk/erlang-github-api>
  
## Contribute
 1) Fork erlang-github-api
 
 2) Write some new features or fix bug
 
 3) Test it
 
 4) Pull request
  
## LICENSE
License in LICENSE file

## More info
More info at kuleshovmail@gmail.com
