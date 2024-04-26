# haskell-translation

A little terminal interface for translating short pieces of text (e.g. blog posts). Interface created with the `brick` library. Work in progress!

To get started, complete the following steps:

1. Pick a directory on your system `/path/to/dir`.
1. Copy the file `file.txt` you want to translate into that directory.
1. Append `.` followed by the language code (e.g. `en` for English) to the file's name, to get e.g. `file.txt.en`.
1. Run `stack run`.
1. Enter the path to the file: `/path/to/dir/file.txt`.
1. Enter the source language, e.g. `en` for English.
1. Enter the target language, e.g. `es` for Spanish.
1. Start translating!

Once you're in the editor, it will show you one sentence of the source material at a time. You can use the up/down arrow keys to move between sentences, and you can translate them one at a time by typing in the editor on the right. You can also press `Shift+Right` to copy over the passage untranslated. Pressing `Esc` will quit the app, and your progress will be saved.

If you are locally serving a LibreTranslate instance on port 5001, the app will hook up with its API and use it to display suggested translations for you. 
