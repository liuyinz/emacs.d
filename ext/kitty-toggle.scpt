#! /usr/bin/env osascript
-- BUG fullscreen shotcut disappear in macos12?

tell application "Kitty"
    activate
    tell application "System Events"
        -- keystroke "f" using {control down, command down}
        keystroke "m" using {control down, command down}
    end tell
end tell
