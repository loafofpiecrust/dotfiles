
// Enable custom styling via the "chrome" folder
user_pref("toolkit.legacyUserProfileCustomizations.stylesheets", true);
user_pref("svg.context-properties.content.enabled", true);

// Scales up the whole interface a bit for my hiDPI screens
user_pref("layout.css.devPixelsPerPx", "1.1");

// Block trackers!
user_pref("browser.contentblocking.category", "strict");
// I use a password manager
user_pref("signon.rememberSignOns", false);

// Default search engine
user_pref("browser.urlbar.placeholderName", "DuckDuckGo");
