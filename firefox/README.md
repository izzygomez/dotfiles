# Firefox Settings Sync

Unfortunately, Firefox does not have a built-in way to sync settings across devices out-of-the-box like Chrome does, so I'm choosing to sync preferences via this dotfiles repo. Was inpsired by [this Reddit thread](https://www.reddit.com/r/unixporn/comments/8izxs5/portable_userchromecss_as_part_of_dotfiles/).

Long story short, I'm overriding the default Firefox profile set via `profiles.ini` to a deterministic location & name, & then symlinking in preferences into that directory. All profiles inside `~/Library/Application Support/Firefox/Profiles` can thus be deleted, as they are not used.
