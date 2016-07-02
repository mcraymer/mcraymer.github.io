--------------------------------------------------------------------------------
 HLTables:  House League Tables
 Version 1.2
--------------------------------------------------------------------------------

HLTables is a simple program that is run in the Command Prompt window. Given a
text file containing a list of players sorted into houses, it creates two text
files, one containing house league score sheets/tables for printing and posting,
and another containing the ranked directory of players. Note that HLTables does
not do any ranking of players. The ranking and sorting into houses must be done
beforehand (see below).

INPUT FILE FORMAT

The input file contains player records (one player per line) that are sorted
into houses with a blank line or comment record between each house. An example
is provide in the file players_2007-10.txt.

Each player record contains the player's last name, first name, home phone and
work phone, each separated by a comma, tab or space(s). First/last names longer
than 15 characters will be truncated. Likewise, phone numbers longer than 14
characters will be truncated.

A comment record is any record with a * in column one.

Two blank lines indicates the end of the sorted list of players to use (I also
use a row of ****** to indicate this but that is only optional). Any further
list of players after this can be used as an archive of players on leave. You
can simply copy/paste them back into the sorted list above when they return.

USAGE

1) Sort and rank players into houses in the input text file using a text editor
like Notepad or WordPad (easier). Use a blank line or comment record between
houses. See below for ranking suggestions.

2) Copy the file HLTables.exe to the directory where your input players file is
located.

3) Double-click on the HLTables12.exe file. It should open a Command Prompt
window.

4) At the prompts enter each of the following followed by the Enter key:

    a)  file name of input players (eg, players_2007-10.txt)
    b)  file name for output score sheets/tables (eg, tables_2007-10.txt)
    c)  file name for output players directory (eg, directory_2007-10.txt)
    d)  date of house league (eg, "October 1-31, 2007")

The program will then generate the two output text files. You can print the
score sheets/tables file using a text editor or word processor. Make sure to use
a monospaced font such as Courier with a font size of about 8 pts. Otherwise the
columns will not line up! The players directory can be copy/pasted into a Word
document. Suggested formatting instructions are given at the top of the file.

PLAYER RANKING

The ranking of players is done according to number of points earned. One point
for each game won and an addition two points for each match won or one point for
a tied match. This is explained in the file League_Rules.doc. At the end of the
month the total points for a player are add up each player.

Generally, the player with the most points in a house gets advanced to the next
higher house while the one with the fewest points gets demoted to the next lower
house. Of course it doesn't always work out this way when trying to maintain
four players per house and players are added and removed from the league each
month. Sometimes three are left in a house to avoid making wholesale changes to
all the houses and for an odd number of players. And sometimes the top two
players in a house are advanced to fill up an empty spot left by a removed
player. Players that don't win a single game are also generally demoted as they
are clearly at too high a level. Finally, inactive players are removed league
each month unless they have a good reason for not playing (eg, illness or
injury).

SUPPORT

For assistance using this software contact:

    Mike Craymer
    Tel. 613-592-9598
    Emai: squash@craymer.com

Created: 2007-09-13
Revised: 2007-09-20
