# Development progress
- [x] Make Internationalisation work
	- [ ] style button
 	- [ ] finish translating and using messages everywhere  
- [x] Make a functional blog with markdown and two languages side by side
	- [ ] database fetch 10 by 10
 	- [ ] infinite scroll with htmx
  	- [ ] better new button
  	- [ ] show which articles are published
- [ ] Make a login by email
	- [ ] custom login page with forgot password instead of new user
 	- [ ] setup email client
  	- [ ] structure databse to support roles
  	- [ ] add command to import basic roles (avoid situation where we delete all roles)
- [ ] Make a way to display files from a folder
- [ ] Make a calendar with similar technology as the blog

# Vision

## Billingual for users and maintainers

By having 2 markdown editors side by side in articles, we make the website billingual not only for users, but also for maintainers, making it way easier to not forget to change both languages, to translate or to fix issues.

## Simple technology that can be used anywhere

By using markdown instead of a html editor, we give tools to users that can be used locally, that can be easily converted to pdf, to a webpage, to a mail, or whatever. People can work from anywhere, share the formated file without issues, past it into deepl without issues, no formatting will be lost ever.

It can also be inserted into latex templates for fancy pdfs.

By exposing a folder in the website, we will allow maintainers to simply drag and drop files in the filer (nextcloud) to upload to the website. Altough this requires discipline on the maintainers side, it does not require any technical skills nor efforts. Which is a great leap forward from phocadownload in jumla.

## Accessible design

Usable on smartphones and on desktops, the website aims to be easy to read, navigate and use from anywhere.

## Login and data managed by us

Comply to swiss laws and try to act responsibly by keeping all the data under our control.

# Technology stack

## Database

At the moment I use SQLite, but I might change to postgresSql to allow easier separation of tables, for security concerns, once we start storing passwords.
## Backend

I use haskell, as strong types make the code safer, and the yesod framework, since it was the most popular and documented framework I found at the time in said language.
It offers type safety and separate the logic of each pages with separate handlers, making it a good server side framework for low maintenance.

The idea is to have a framework that lets me directly do thinks with the provided language so I can have tools for our tasks completely compatible with each other. No more copying rich text from program A to html editor in program B, manually fixing syntax, and repeating for the second language.

## Frontend

I do minimal usage of htmx, the idea is that htmx, when used lightly, gives benefits almost for no effort and added complexity. So I will abuse functionalities like hx-boost that allow navigating without javascript while offering more performance to javascript users without complexity.

The rest is all css and html compiled from shakespear template which gives sytactic sugar and variable interpolation with type safety. So it should be very easy to change the layout/content for people without experience.

# Actual features

## Internationalisation

German and French everywhere (technology could allow italian on static pages but much work on blog obviously)

## Blog

Works with yesod forms, markdown and the database, sorted by date.

# Future features

## Calendar

Works with yesod forms, markdown and the database, sorted by date.

## Login

Works with yesod auth, saves **hashed and salted passwords** to the database.
Does not support creating accounts as every account should be in a database, as we only allow members.

## Files

A file explorer that lets download the content of a folder for visitors, another for members.
the content is filtered by language.

# Installation for developpement

## Haskell Setup

1. If you haven't already, [install Stack](https://haskell-lang.org/get-started)
	* On POSIX systems, this is usually `curl -sSL https://get.haskellstack.org/ | sh`
2. Install the `yesod` command line tool: `stack install yesod-bin --install-ghc`
3. Build libraries: `stack build`

If you have trouble, refer to the [Yesod Quickstart guide](https://www.yesodweb.com/page/quickstart) for additional detail.

## Development

Start a development server with:

```
stack exec -- yesod devel
```

As your code changes, your site will be automatically recompiled and redeployed to localhost.

## Tests

```
stack test --flag ssssgh:library-only --flag ssssgh:dev
```

(Because `yesod devel` passes the `library-only` and `dev` flags, matching those flags means you don't need to recompile between tests and development, and it disables optimization to speed up your test compile times).

## Documentation

* Read the [Yesod Book](https://www.yesodweb.com/book) online for free
* Check [Stackage](http://stackage.org/) for documentation on the packages in your LTS Haskell version, or [search it using Hoogle](https://www.stackage.org/lts/hoogle?q=). Tip: Your LTS version is in your `stack.yaml` file.
* For local documentation, use:
	* `stack haddock --open` to generate Haddock documentation for your dependencies, and open that documentation in a browser
	* `stack hoogle <function, module or type signature>` to generate a Hoogle database and search for your query
* The [Yesod cookbook](https://github.com/yesodweb/yesod-cookbook) has sample code for various needs

## Getting Help

* Ask questions on [Stack Overflow, using the Yesod or Haskell tags](https://stackoverflow.com/questions/tagged/yesod+haskell)
* Ask the [Yesod Google Group](https://groups.google.com/forum/#!forum/yesodweb)
* There are several chatrooms you can ask for help:
	* For IRC, try Freenode#yesod and Freenode#haskell
	* [Functional Programming Slack](https://fpchat-invite.herokuapp.com/), in the #haskell, #haskell-beginners, or #yesod channels.
