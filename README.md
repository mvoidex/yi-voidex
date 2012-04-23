yi-voidex
=========

My config for Yi editor

It also supports menu-mode.

Just define simple menu:

<pre>
mainMenu = [
  menu "File" [
    actionY_ "Quit" askQuitEditor,
    actionY "Save" (fwriteBufferE . parentBuffer)],
  menu "View" [
    menu "Windows" [
      actionE_ "Next" nextWinE,
      actionE_ "Previous" prevWinE,
      actionE_ "Split" splitE]]]
</pre>

and then use `startMenu` function in Keymap

