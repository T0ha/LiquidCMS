# LiquidCMS

![Erlang](https://img.shields.io/badge/Erlang%2FOTP-28.1.1-red?logo=erlang)
![Nitrogen](https://img.shields.io/badge/Nitrogen-3.0.0--alpha.13-blue)
![Cowboy](https://img.shields.io/badge/Cowboy-2.12.0-green)

A block-based content management system built with Erlang/OTP and the Nitrogen Web Framework.

Pages are composed from reusable blocks using the MFA (Module-Function-Arguments) pattern, with an admin panel for managing pages, blocks, templates, assets, users, and roles. Data is stored in Mnesia with a Bootstrap-based frontend.

## Features

**Content Management**
- Block-based page composition via MFA records
- WYSIWYG editor with customizable toolbar
- Template system with error pages (403, 404, 500)
- Multi-language support (i18n)
- XML sitemap generation

**Administration**
- Admin panel with CRUD interface for all entities
- Role-based access control (admin, editor, nobody)
- Database backup and restore
- Email form management

**User Management**
- Authentication with email confirmation
- Password reset via SMTP
- Role assignment per user

**Frontend & Assets**
- Asset management with CSS/JS minification
- Bootstrap UI (via Coldstrap)
- Configurable analytics (Google, Yandex, Hubspot)
- Social sharing integration
- Custom 404 page handling

**Developer Experience**
- Hot code reloading with [sync](https://github.com/rustyio/sync)
- Auto-install: first run creates Mnesia schema, tables, and seeds defaults
- Pluggable collection modules

## Prerequisites

- **Erlang/OTP 28.1.1** — a `.tool-versions` file is included for [asdf](https://asdf-vm.com/)
- **Rebar3**
- **Git**

## Getting Started

```bash
git clone git@github.com:T0ha/LiquidCMS.git
cd LiquidCMS
rebar3 compile
rebar3 shell
```

- `rebar3 compile` fetches dependencies and copies Nitrogen static assets (via post-compile hook)
- `rebar3 shell` starts the `nitrogen` application using `etc/sys.config`
- On first run, the database is automatically initialized with schema, tables, and default data
- The server is available at **http://localhost:8000**

## Project Structure

```
LiquidCMS/
├── etc/                    # Configuration files
│   └── sys.config          # OTP application config (port, paths, limits)
├── include/                # Header files (.hrl)
│   ├── cms.hrl             # CMS records (cms_mfa, cms_page, cms_user, etc.)
│   ├── db.hrl              # Database macros
│   └── records.hrl         # Custom element records
├── priv/                   # Private data
│   └── cms.config          # CMS smart extensions config
├── sample/                 # Sample configuration files
│   └── esmtp.config        # SMTP setup template (Gmail example)
├── src/
│   ├── cms_modules/        # Core CMS modules (admin, account, sitemap)
│   ├── collections/        # Pluggable content blocks (bootstrap, html5, router, analytics, social, ...)
│   ├── elements/           # Custom Nitrogen elements (CRUD, WYSIWYG)
│   ├── actions/            # Custom Nitrogen actions
│   ├── index.erl           # Main routing and page rendering
│   ├── db.erl              # Mnesia database operations
│   ├── nitrogen_main_handler.erl
│   └── ...
├── static/                 # Static assets (CSS, JS, images, fonts)
└── templates/              # HTML templates (error pages, mail, analytics)
```

## Configuration

### Server (`etc/sys.config`)

| Setting        | Default       | Description                     |
|----------------|---------------|---------------------------------|
| `address`      | `0.0.0.0`    | Bind address                    |
| `port`         | `8000`        | HTTP port                       |
| `document_root`| `./static`    | Static file directory           |
| `max_post_size`| `100000000`   | Max POST body size (~100 MB)    |
| `max_file_size`| `100000000`   | Max upload file size (~100 MB)  |

### SMTP (`sample/esmtp.config`)

Copy `sample/esmtp.config` to `etc/esmtp.config` and fill in your SMTP credentials. The sample is pre-configured for Gmail (port 465, SSL).

### CMS Extensions (`priv/cms.config`)

Defines smart extensions for features like automatic sitemap XML generation.

## Architecture

**MFA Block Pattern** — Pages are built from `#cms_mfa{}` records, each holding `{Module, Function, Args}`. At render time, the page module calls each block's MFA in sort order, producing a composable waterfall of Nitrogen elements.

**Collections** — Pluggable modules in `src/collections/` that provide groups of related blocks (e.g., `bootstrap`, `html5`, `analytics`, `social`, `router`). Each collection exports functions that can be referenced as MFA blocks.

**Database** — Mnesia with `disc_copies` storage. Tables: `cms_settings`, `cms_mfa`, `cms_template`, `cms_asset`, `cms_page`, `cms_user`, `cms_role`, `cms_form`, `cms_language`.

**Roles** — Priority-based: admin (1000), editor (2000), nobody (10000). Lower number = higher privilege. Access control checks user role priority against page requirements.

**Request Flow** — Cowboy → Simple Bridge → `nitrogen_main_handler` → page module → MFA block waterfall → rendered HTML.

## Contributing

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/my-feature`)
3. Commit your changes
4. Push to the branch and open a Pull Request

For development, `rebar3 shell` starts with `sync` enabled for automatic hot code reloading on file changes.

## License

This project does not currently include a license file. Please contact the maintainer for licensing information.
