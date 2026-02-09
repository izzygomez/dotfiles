# Firefox Settings Sync

Unfortunately, Firefox does not have a built-in way to sync settings across devices out-of-the-box like Chrome does, so I'm choosing to sync preferences via this dotfiles repo. Was inspired by [this Reddit thread](https://www.reddit.com/r/unixporn/comments/8izxs5/portable_userchromecss_as_part_of_dotfiles/).

Here's a quick overview of the files in this directory, where `{FIREFOX_DIR} == ~/Library/Application Support/Firefox` on macOS:

- `profiles.ini`: This configures where the Firefox profile directory lives. By default, Firefox creates profiles inside `{FIREFOX_DIR}/Profiles/` with random names, so I'm using this file to deterministically set the profile name & location to `{FIREFOX_DIR}/izzys-firefox-profile/`. Other profiles inside `{FIREFOX_DIR}/Profiles/` can thus be deleted.
- `user.js`: This sets preferences for Firefox. It is read first & overrides anything in `pref.js` inside the profile directory. Should only contain preferences that should be synced across all devices that use this dotfiles repo.
- `chrome/user{Chrome,Content}.css`: These files are used to set the appearance of Firefox. They are CSS files that contain styles that are applied to the Firefox user interface. The `userChrome.css` file is used to style the browser's chrome (the interface elements that surround the web page), while the `userContent.css` file is used to style the content of web pages.

Tips for syncing with a new machine:

- If the new machine has a fresh install of Firefox, simply run the dotfiles `install-dotfiles.sh` script & Firefox will automatically pick up the new profile.
- If the new machine has an existing Firefox profile with data (i.e. browsing history, cookies, etc.), copy the contents of the profile directory into a new directory, & then run the dotfiles `install-dotfiles.sh` script. This will create a new profile with the same data as the old one, but with the synced custom settings.

```bash
cd {FIREFOX_DIR}/Profiles
mdkir izzys-firefox-profile
cp -r Profiles/{random-profile-name}/* izzys-firefox-profile/
~/code/dotfiles/install-dotfiles.sh
# optionally, delete old profile(s)
rm -r Profiles/{random-profile-name}
```
