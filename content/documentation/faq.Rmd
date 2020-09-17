---
title: Troubleshooting
date: 2016-04-10
type: book
weight: 170
summary: Troubleshoot common issues.
---

Some common questions and answers are listed below.

**Changes do not show on my site**

Login to Netlify and check the status of the latest deploy in the _Deploys_ tab.

If there is a progress indicator, Netlify is still updating your site.

If there is a red cross, click on the failed deploy and check the error message. You can either revert the change which broke the site, or consider addressing the issue.

Feel free to join the Wowchemy community chat (link in the menu above) and post a detailed description of the issue with the error message and a link to your GitHub repository.

**Error: Homepage not found at `/home/index.md`**

Possible causes:

- A Widget Page was modified whilst running Hugo Server
  - Stop Hugo Server and then restart it
- `content/home/index.md` is missing
  - Add the `/home/index.md` homepage file to each language's content folder
  - For example, your site should have a `content/home/` folder containing `index.md` and your homepage sections, or for multi-language sites, `content/en/home/` and `content/zh/home/` etc.
  - Refer to the 'Build Your Homepage' and 'Language' documentation pages as well as the example homepage file at https://github.com/gcushen/hugo-academic/tree/master/exampleSite/content/home/index.md

**Error: failed to download modules: go command failed: go: unknown subcommand "mod"**

Login to Netlify and set `GO_VERSION` to `1.12` in your _Environment_ settings.

Then click _Deploys_ tab, and from the _Trigger Deploy_ dropdown, choose _Deploy Site_.

**File not found errors after updating**

Depending on the specific error, it may be resolved by clearing Hugo's module cache:

```sh
hugo mod clean
hugo server
```

**I cloned/downloaded Wowchemy but Hugo produces errors when using it with my existing Hugo site**

Wowchemy is a website *framework* rather than just a *theme*. Please follow the step-by-step guide on the Installation and Getting Started pages of the documentation.
 
If you experience issues, first try running Hugo on an unedited template and then compare the configuration parameters in the demo site's `config/` folder and front matter of content files with the files in your site.

Feel free to join the Wowchemy community chat (link in the menu above) and post a detailed description of the issue with the error message and a link to your GitHub repository.

**Hosting your site with Netlify or Cloudflare and experience strange behavior such as filters not working?**

Disable post-processing steps such as *minification* in your Netlify/Cloudflare admin panel.

**Publications and other content are not sorted by newest first**

Hugo requires a valid date field, as per the front matter of the Demo publications in `themes/academic/content/publication/`. If you wish to display a partial date, such as just the year, a full valid date should still be entered in the front matter - the Customization page describes how to change the format in which dates are displayed on your site.
