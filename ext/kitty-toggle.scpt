#! /usr/bin/env osascript

tell application "Kitty"
    activate
    tell application "System Events"
        keystroke "f" using {control down, command down}
    end tell
end tell
