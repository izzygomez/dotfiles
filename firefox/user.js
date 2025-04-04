// This customizes the toolbar UI.
user_pref(
  "browser.uiCustomization.state",
  '{"placements":{"widget-overflow-fixed-list":[],"unified-extensions-area":["_news-feed-eradicator-browser-action","myallychou_gmail_com-browser-action","markdown-viewer_outofindex_com-browser-action","_92e6fe1c-6e1d-44e1-8bc6-d309e59406af_-browser-action","_74145f27-f039-47ce-a470-a662b129930a_-browser-action"],"nav-bar":["back-button","forward-button","vertical-spacer","stop-reload-button","urlbar-container","_446900e4-71c2-419f-a6a7-df9c091e268b_-browser-action","_d634138d-c276-4fc8-924b-40a0ea21d284_-browser-action","admin_2fas_com-browser-action","reset-pbm-toolbar-button","_3c6bf0cc-3ae2-42fb-9993-0d33104fdcaf_-browser-action","jid1-mnnxcxisbpnsxq_jetpack-browser-action","ublock0_raymondhill_net-browser-action","search_kagi_com-browser-action","addon_darkreader_org-browser-action","unified-extensions-button","downloads-button","developer-button","fxa-toolbar-menu-button"],"TabsToolbar":["tabbrowser-tabs","new-tab-button","alltabs-button"],"vertical-tabs":[],"PersonalToolbar":["personal-bookmarks"]},"seen":["save-to-pocket-button","developer-button","_446900e4-71c2-419f-a6a7-df9c091e268b_-browser-action","jid1-mnnxcxisbpnsxq_jetpack-browser-action","admin_2fas_com-browser-action","_news-feed-eradicator-browser-action","ublock0_raymondhill_net-browser-action","myallychou_gmail_com-browser-action","_3c6bf0cc-3ae2-42fb-9993-0d33104fdcaf_-browser-action","addon_darkreader_org-browser-action","markdown-viewer_outofindex_com-browser-action","search_kagi_com-browser-action","_92e6fe1c-6e1d-44e1-8bc6-d309e59406af_-browser-action","_d634138d-c276-4fc8-924b-40a0ea21d284_-browser-action","_74145f27-f039-47ce-a470-a662b129930a_-browser-action"],"dirtyAreaCache":["nav-bar","PersonalToolbar","TabsToolbar","vertical-tabs","unified-extensions-area"],"currentVersion":21,"newElementCount":15}'
);
// Enables use of user{Chrome,Content).css files in the profile directory.
user_pref("toolkit.legacyUserProfileCustomizations.stylesheets", true);

// Necessary to test changes to Firefox CSS files live. Used in combination
// with "Browser Toolbox" [1] (Cmd-Opt-Shift-I on Mac).
// [1] https://firefox-source-docs.mozilla.org/devtools-user/browser_toolbox/index.html
user_pref("devtools.debugger.remote-enabled", true);
user_pref("devtools.chrome.enabled", true);

//// Find bar settings ////
// Enables the floating find bar.
user_pref("userchrome.floating-findbar-on-right.enabled", true);
// Default to highlighting all matches in the find bar.
user_pref("findbar.highlightAll", true);
// Disable sound when search fails in the find bar.
user_pref("accessibility.typeaheadfind.enablesound", false);
