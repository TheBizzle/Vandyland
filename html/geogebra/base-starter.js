window.baseStarter =
  { id:                    "ggbApplet"
  , showMenuBar:           true
  , showAlgebraInput:      true
  , showToolBar:           true
  , customToolBar:         "0 73 62 | 1 501 67 , 5 19 , 72 75 76 | 2 15 45 , 18 65 , 7 37 | 4 3 8 9 , 13 44 , 58 , 47 | 16 51 64 , 70 | 10 34 53 11 , 24  20 22 , 21 23 | 55 56 57 , 12 | 36 46 , 38 49  50 , 71  14  68 | 30 29 54 32 31 33 | 25 17 26 60 52 61 | 40 41 42 , 27 28 35 , 6"
  , showToolBarHelp:       true
  , showResetIcon:         false
  , enableLabelDrags:      false
  , enableShiftDragZoom:   false
  , enableRightClick:      false
  , errorDialogsActive:    false
  , useBrowserForJS:       false
  , allowStyleBar:         false
  , preventFocus:          false
  , showZoomButtons:       true
  , capturingThreshold:    3
  // add code here to run when the applet starts
  , appletOnLoad:          (api) => { /* api.evalCommand('Segment((1,2),(3,4))');*/ }
  , showFullscreenButton:  true
  , scale:                 1
  , disableAutoScale:      false
  , allowUpscale:          false
  , clickToLoad:           false
  , appName:               "classic"
  , showSuggestionButtons: true
  , buttonRounding:        0.7
  , buttonShadows:         false
  , language:              "en"
  // use this instead of ggbBase64 to load a material from geogebra.org
  // material_id:"RHYH3UQ8"
  // use this instead of ggbBase64 to load a .ggb file
  // filename:"myfile.ggb"
  , ggbBase64:             "UEsDBBQACAgIAMGSE1EAAAAAAAAAAAAAAAAXAAAAZ2VvZ2VicmFfZGVmYXVsdHMyZC54bWztmktv2zgQgM/bX0HwtHuILcqWH0GUIi2w2AJpGmyCYq+0RMvc0KRWpGI5v74UKUtybWVjxamdtDmEGorPb4bDIeWz99mcgXuSSCq4D1HHgYDwQISURz5M1fRkBN+fvzuLiIjIJMFgKpI5Vj708pJlPS11vPE4z8Nx7MOAYSlpAEHMsMqr+HABAcgkPeXiCs+JjHFAboIZmeNLEWBlWpkpFZ92u4vForPqryOSqKublN1Mht0oUh2dQqAHzaUPi4dT3e5a7UXP1HMdB3X/+Xxp+zmhXCrMAwKBnlBIpjhlSupHwsiccAXUMiY+jAXlCgKGJ4T58DqXwO/ThJA/ICgqaU4OPH/325mciQUQk39JoPNUkpKynhG6eRn9+qNgIgGJD4dDCCKbTHzoep7GxeIZ9qFjCzO8JAm4x6zMwakSgalvcqeYSbIqq3v6LEJi3/SL8pzODU4gFdGacDoIAhkTEupRw2KOyChmaXRcazEQIgklyHx4ha8gWBbpg01NEUPnhj4UnXr1XLVkpDb2s24B9mmIQxITHupCa5xRK86DkeGcJxObvDTml4Tcf2nIg1+QmyCj3Sl/4XW2biu2yPUMXJP+KFfxKhzFJ/43ifSY64x7vxjvlfG6Bfdb0XUMW+eVkjVFLEOZ/9cRjZjHjGR7BM8oryBeGqGE7raLL+rQnQPFFk5r6DkQi0/NaHDHiZQ526rd/OEvGur9y/QndAxJlW4JDUe2BfIfX1Ma1TqjuszjipimPFDGpRRwP6bJfV0bvb5zCH1UbbZeAQ3KeC7pZpaSRLlUcrlZyZVptwvpfnbTFqliec+fuNKHLmIMVm5M7o6Q+FY39YXfJpjL/OS1bkvNmkvw8jGtea9Baz+bzlae6+orTkpNpDrAn+qxh3X1tQuRGjfxjusdWoc7+POtRJ4f1hyVQe9orfsxq0E7r+A6/e0YO8MjNqt7PT1R8fhaiFWU8CpitiNzkVsCbZwoIinm/3dsYcuotsavV3Kpj6HVx/PHuPPB0usZnXpow76RY/9Qf+wgNEDuodX8OOC1I8p1mVEhRgdCfLTRYDPPQPD8Qnx1xLBSSbL/xpzHHs5yNCLc+lwJQOaYYkvHVH5wik8VGTLyEpm3D8hmm/p64AnNwIWtcWELXrg26dmkbxOvBNTuAGlUG2u/VQugv9sc+u1OPa/JlbxJpf+AIJ6nc5LUXMPVSi6Nx7POQbeXkjXVPsEVNNlJs1VIRkNtQnOqlXSitTfHmdEinkjBUkVugoQQXn21s2a8oKGa5aGd7ntKs9xcbJtgJhL6ILgqaYB8FVww831v7ZZjm/m4j4Wwa8b6PPeMecSq1XhhpUoD9hrfFPr+hm+bYuoMnQLhoOOOemjk9ZwhGo690eCJSNGoQmpfPJnopn0gZw8WstM6d7etc5wE1fVpz9nueJyOg4Z9r+eOXQ+Nx3394O3/cPhnmVEda47xts9YwEbRF7vIYyJIZXU9baWS0OiNhSs4zSijOFk+z9Z3IqxIVgUMt0ao/b7gCAE3T0Vjj6qhfbJS7SO+ncyUaoocz3UF2wnlH3BwFyUi5eHmNrSXqaND21YztIkQjODKEX1YybWPxxsbfxOgYq895OoLZiS4m4hsba963MdQWa2ASyPUPupuWQFPn+XmPndycFNoc0nX9K1xayRSJ92t/cCpu/o11fk3UEsHCFPsFDbtBAAA7yUAAFBLAwQUAAgICADBkhNRAAAAAAAAAAAAAAAAFwAAAGdlb2dlYnJhX2RlZmF1bHRzM2QueG1s7ZjdbtMwFMev4Sks39PYaZIt0zJUwQVIMA1xw62XnLaGxA62uzZ7Nd6BZ+LEzroUVsSqbRKIXuz46xzbv797au/05aapyRUYK7UqKJ8wSkCVupJqUdCVm784pi/Pnp8uQC/g0ggy16YRrqBpP3Lrh7VJmud9m2jbgpa1sFaWlLS1cL1LQdeUkI2VJ0qfiwZsK0r4WC6hEe90KZyPsnSuPYmi9Xo9uZlvos0iwpA22tgqWizcBC0luGhlCzoUTjDujvd66v1ixnj06f27MM8LqawTqgRKcEMVzMWqdhaLUEMDyhHXtYBL10qWU5yjFpdQF/StcrhLKPslknJlrtB/cC7olKeMnj1/dmqXek305WccV1BnVrD195WoH4Pdr3StDTEFjTklCJgztJdo8xjJ1e1SFJRNOAsfnuSM84zHwb8WHRhyJTAoCy1i5XTpQ/rWuagt3IzFyd/rCkJPMoxXsvGsiXWAMuHktgWofClsn3nNOi//OJ5U8NF1NRC3lOUXBRbxpyOnvvBGVhX0pyj4gFyAukIi2ljUnvlZOuaHX7PhsG24r3fc917z0Oz9calGbsgseMzCwFkczDSYJJh0iwS+qrBO2/8taCsMHjcMVPb9p9Eg9i+yi420I9VnffX1jtJsepDSzAvNvMzsVuTHkhRPz+OKup8vGcqAu/7+7fe4/RepFMaBlUKNwL/qO34mn/0N5B+T+36QGF/BiN+Fr+/wwzR4EL889wBjnnuE3m5zVPpQGEutTWXJpqDn4hwTwWCvB7sO1g+di/5naZhtb5a8Cy47EK6uuyVURqtbvqOmW8TTAfEh36j7ysLTqdcl5T+f7EkyIEnzjCVZ8mAaHXrU70V2ZsqlbKACsYsWhX0qtDEPP8vJkUfbm3+D7UWHmVlWu1yf7sj61IGLzwPX+J85sxdG2maXKn9CqllI0IFqnv2VVBW47T7P+/I4q6b/s+p9WH5dicrfxIatfripj5nyAx8s+1NjluT95yjj6TFPYv5QgB7j0XHnk6NvDO+KLpjreBvwvq8QMsuCOQrmOJh87wtFNm0tS+l+L61dmTk+me+6Mg9duyonh6mMfndemidHf3rsbwM/ybWZ/+nNLhq9+KObfy+c/QBQSwcIlQmlEkoDAAAAEQAAUEsDBBQACAgIAMGSE1EAAAAAAAAAAAAAAAAWAAAAZ2VvZ2VicmFfamF2YXNjcmlwdC5qc0srzUsuyczPU0hPT/LP88zLLNHQVKiuBQBQSwcI1je9uRkAAAAXAAAAUEsDBBQACAgIAMGSE1EAAAAAAAAAAAAAAAAMAAAAZ2VvZ2VicmEueG1svVdRb9s2EH5uf8VBz0lMUqIkF3aLtEuBAm1XLNsw7E2WGJuILAoiHTtDf/zuSEmW0wxrlmJJGJLH4919d0cetXhz2NZwpzqrTbOM+AWLQDWlqXSzXkY7d3OeR29ev1yslVmrVVfAjem2hVtGkjjHfTi7kPM50Yq2XUZlXVirywjaunC0ZRntI9DVMkpFfHmZyffnIonZeZLNr87zhF2dX8mf2HuZvruaCxkBHKx+1ZjPxVbZtijVdblR2+KjKQvn9W2ca1/NZvv9/mKw7MJ06xkqt7ODrWbr9eoC+wgQXmOXUT94hXJPdu9jv08wxmd/fPoY9JzrxrqiKVUEBH2nX798sdjrpjJ72OvKbdBRWYxYN0qvN+iMOYsjmBFXix5pVen0nbK4dzL16N22jTxb0dD6izCCegQWQaXvdKW6ZcQumMxyFidpLljO5iLJkwhMp1Xjem7ea50N8hZ3Wu2DYBp5nQmbZxgqbfWqVsvopqgtAtPNTYfeHefW3ddqVaBe1+1wfjSJn/lfZNF/KRKHwIMzcI2xM2oZNilZMGeiW3IRgTOm9pIZfAUOkmEDPoczSDOkCOASEqTkSMkgJprkCcRALDyGJME+ITJPcYWW8T+qA85xBQQDIUBwEDFOpQSJbBntFcibzr08ho240SJsMdHiGJunxQk2QSMUJIMYtEPGqR9J/z+nPahFCsLhl5CWzFEdEWTGIUZLcJ4xQLkxKeEeTcKA/jgkpERkIHLwUr18Jp4Snp7wID5DdORj0Umx+bA9iE5yGhsMBUNsZ9Tx0IlAZWHK4tCJ0CWhk4EnCTuTwBqAsiTwJPFzEQ744qfgyyf4OIHAeJD1vouB7ObefuqSfpqGqU83xllPzQN1TtP0mWDi/wSGT7SGI/oUpYNKnqRP0Pm8xDzizJ+C85nufdS5Eq8n+vPtG5Xxk3B+cz8OGsVcfK/G9OTo/RjASf7dgLnI/3edGXv0tgk97/sfE4j5v4R+MRvK5KK3COyGePtz5dTWko1ZDKkYa1ZKJaUvXJmATEKWTsrXGRWwVB5rGFWw/KSGyfy0kKVEzHxVxIpBNShUNJEMRe2sL2tfvylrWH+SYwlCA0kUB8DCCSndVn0tQivEWI2EpIIkUsCKJQWkdCP+Q2HCp5uxenTsRtXtGALvQ920O3fit3JbDUNnHnBXprx9O/q5X1GFdVM2fMcc30vhXXPynHqxqIuVqvFpek1JAHBX1HR8vIYb0zgYbrk00ErT2C+dce9Mvds2FqA0NRvNNTWfjMVoCU7iyUIyXZCThXQyzgamE70GV2BnFeo3nR3lFFX1gViOOY1e+bmp7992qrhtjW6cnchbzPwTdKF2Za0rXTS/Y/YOj73Pu+1KdeCHhkLlDSA3wONv1Zxlg5Gmq67vLWY7HP5UHe5Opbhgkx8M/H1YSTjeGLYsav/GOGHyXONSkK3urpVzGCgLxUHZwVHrTlfT8Qf71tTV6AaP/F3Rul3nPz+wgHRk9GWzrpUPuc9FfKOXtytzuA63bhpk/Xrf0q0f9K/W3uWAt4SQ+D2x7vtV6D0PGTZyMc/DPEePgYSO65zu9nXfr0LvuTAbg2k9UD6gZIMWbSHMTw6Lz2R67+8a7T4OE6fL2yNQ4g/xHTx4KpL/IJGL2YPUWhS1/6YaEm1rKhUOWxz4T9YXt6prVN0nPkZ9Z3Y2sE/OBB6DL4XbXDbVL2qN18qXgq51h4YE1iO+SpV6ixsDvfdzQTnwGwIL1EqtOzX4IxgTotBbCbbFs1TZjVJujEXI+CMbC2AG8xeuwMLjC9ZW47V3jqHeFgf/DMJT0vana2HLTreU3LDC4nOrjglcaUsiqunhpYOO2Eq6SjEYjgKBV8DObUznv/cKRxQyZMrqj33/afv6b1BLBwgxzpoxhgUAAKYPAABQSwECFAAUAAgICADBkhNRU+wUNu0EAADvJQAAFwAAAAAAAAAAAAAAAAAAAAAAZ2VvZ2VicmFfZGVmYXVsdHMyZC54bWxQSwECFAAUAAgICADBkhNRlQmlEkoDAAAAEQAAFwAAAAAAAAAAAAAAAAAyBQAAZ2VvZ2VicmFfZGVmYXVsdHMzZC54bWxQSwECFAAUAAgICADBkhNR1je9uRkAAAAXAAAAFgAAAAAAAAAAAAAAAADBCAAAZ2VvZ2VicmFfamF2YXNjcmlwdC5qc1BLAQIUABQACAgIAMGSE1ExzpoxhgUAAKYPAAAMAAAAAAAAAAAAAAAAAB4JAABnZW9nZWJyYS54bWxQSwUGAAAAAAQABAAIAQAA3g4AAAAA"
  };
