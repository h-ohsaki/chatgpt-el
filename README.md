# NAME

chatgpt-el - interactively access AIs (ChatGPT/Gemini/Claude/DeepSeek) from Emacs

![video](screenshot/video.gif)

# DESCRIPTION

**chatgpt-el** is an Emacs Lisp program designed to interactively access AIs
(e.g., ChatGPT, Gemini and Claude) from within Emacs.  While those AIs can be
accessed via API interfaces using a programming language like Python and
several modules, such access has several drawbacks.

1. Batch processing through APIs is slow; accessing APIs is slow, and
   responses can often take an unnecessarily long time.  Such slow response
   times can make accessing AIs frustrating.

2. Accessing AIs via APIs sometimes requires a non-free AI account with credit
   card registration.  Frequent use of AIs can lead to high costs and require
   monitoring of AI billing records.

**chatgpt-el** solves the above issues by enabling access to AIs within Emacs
without the use of an API key.  The program is implemented using
Chromium/Chrome browser's CDP (Chrome DevTools Protocol)
(https://chromedevtools.github.io/devtools-protocol/), and therefore requires
a CDP-enabled Chromium/Chrome browser to be running.  **chatgpt-el** operates
by remotely controlling your instance of Chromium/Chrome using the Python
script called `chatgpt-cdp`, which is built on the websocket-client library
(https://pypi.org/project/websocket-client/). Therefore, your Chromium/Chrome
browser must accept a CDP connection from the `chatgpt-cdp` script.

![overview](overview.png)

Note that the implementation of the `chatgpt-cdp` script depends on the
internal structure of the HTML file returned by the AIs. If `chatgpt-cdp` does
not work in your environment, you may need to modify the program according to
your environment.

# PREREQUISITES

- Chromium or Chrome browser (or other browsers supporting the CDP protocol).
- Python
- websocket-client module (https://pypi.org/project/websocket-client/)

# INSTALLATION

``` sh
> git clone https://github.com/h-ohsaki/chatgpt-el.git
> cd chatgpt-el
> pip install websocket-client perlcompat tbdump
> sudo install -m 644 chatgpt.el /usr/local/share/emacs/site-lisp
> cat <<EOF >>~/.emacs
;; chatgpt-el
(autoload 'chatgpt-query "chatgpt" nil t)
(autoload 'chatgpt-insert-reply "chatgpt" nil t)
(autoload 'chatgpt-fill-at-point "chatgpt" nil t)
(global-set-key "\C-cb" 'chatgpt-query)
(global-set-key "\C-cQ" 'chatgpt-insert-reply)
(global-set-key "\C-cf" 'chatgpt-fill-at-point)
(setq chatgpt-engine "ChatGPT")
(setq chatgpt-prog "../path/to/chatgpt-el/chatgpt-cdp")
EOF
> mkdir -p ~/var/log/chatgpt
```

You can place `chatgpt-cdp` script anywhere in your system.

# USAGE

1. Start Chromium/Chrome browser with remote debugging on port 9000.

``` sh
> chromium --remote-debugging-port=9000 --remote-allow-origins=http://127.0.0.1:9000
```

2. Visit ChatGPT/Gemini/Claude/DeepSeek in Chromium/Chrome, and login with
   your account.

3. In Emacs, move the point (i.e., the cursor in Emacs) at the end of the
   query text.  Alternatively, you can select the region containing the query
   text.  Then, type `C-c b` or execute `M-x chatgpt-query`.  With `C-u C-c
   b`, you can revise the query in the minibuffer.  With `C-u C-u C-c q`, you
   will be prompted what query prefix is prepended to the query text.

4. The query is automatically submitted to the AI in your Chromium/Chrome.
   The reply from the AI will be displayed in another buffer in Emacs.

5. Once the reply is displayed, type `C-c Q` or execute M-x
   chatgpt-insert-reply from Emacs.  The reply from the AI is inserted at the
   current point.  Use `C-u C-c Q` to insert both the query text and the reply
   from the AI.

6. When requesting translation or proofreading of a text for the AI, it is
   handy to replace the query text with the reply from the Ai. To do this,
   type `C-u C-u C-c Q`.

# TROUBLE SHOOTING

1. Make sure your Chromium/Chrome accepts a CDP connection on localhost:9000.
   
``` sh
> telnet localhost 9000
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
GET /json/version HTTP/1.1

HTTP/1.1 200 OK
Content-Security-Policy:frame-ancestors 'none'
Content-Length:391
Content-Type:application/json; charset=UTF-8

{
   "Browser": "Chrome/112.0.5615.121",
   "Protocol-Version": "1.3",
   "User-Agent": "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/112.0.0.0 Safari/537.36",
   "V8-Version": "11.2.214.14",
   "WebKit-Version": "537.36 (@39cc...2268)",
   "webSocketDebuggerUrl": "ws:///devtools/browser/e32f...ca87"
}
```

2. Make sure your Chrome/Chromium visits ChatGPT/Gemini/Claude/DeepSeek page
by running `chatgpt -i`.

``` sh
> ./chatgpt-cdp -i
> ./chatgpt-cdp -e gemini -i
> ./chatgpt-cdp -e claude -i
> ./chatgpt-cdp -e deepseek -i
```

3. Send a query (e.g., `hello`) to the AI.

``` sh
> ./chatgpt-cdp -s hello
> ./chatgpt-cdp -r
<div class="flex w-full flex-col gap-1 empty:hidden first:pt-[3px]"><div class="markdown prose dark:prose-invert w-full break-words dark"><p data-start="0" data-end="37" data-is-last-node="" data-is-only-node="">Hello! 😊<br data-start="9" data-end="12">
How can I help you today?</p></div></div>
> ./chatgpt-cdp -e gemini -s hello
> ./chatgpt-cdp -e gemini -r
<div _ngcontent-ng-c3682768483="" class="markdown markdown-main-panel stronger enable-updated-hr-color" id="model-response-message-contentr_d1f3fd77a101a596" dir="ltr" style="--animation-duration: 600ms; --fade-animation-function: linear;"><p data-sourcepos="1:1-1:32">Hello! How can I help you today?</p></div>
```

This will show the reply from the AI, which must be equivalent to that shown
in your Chromium/Chrome.

# FOR QUTEBROWSER USERS

If you are a qutebrowser user instead of Chromium/Chrome, you can use
**qutechat** (https://github.com/h-ohsaki/qutechat), an extension to
**chatgpt-el** supporting qutebrowser.
