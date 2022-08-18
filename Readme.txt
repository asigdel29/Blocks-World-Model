The project was implemented with three tasks:

Task 1:Improve User Interaction
The first task involves adding three niceties to the program, focusing on the natural language processing, natural language generation, and overall usability of the system. This task is designed to help you get a better feel for how the program fits together.

Start by improving the feedback the program gives when something goes wrong. if reference resolution fails, the system should tell the user what it can’t resolve. If parsing fails, the system should tell the user that it doesn’t understand the input. There are more opportunities to do nice things, but these two are some minimal expectations.
Add an ‘exit’ command so that you can get out of the talktome rule.

Task 2: Improve Reference Resolution
Consider the following sequence of events, beginning from the initial state of all of the blocks on the table.

Stack the small red block on the small blue block
Stack the large red block on the large green block
*Pickup the red block from the blue block
This last case doesn’t work because our reference resolution is not sophisticated enough to take the stacking context into account.


Task 3: Heuristic Planning

InitialState = [on(b1, table), on(b2, table), on(b3, table), on(b4, table), on(b5, table),on(b6, table), on(b7, table), on(b8, table)]
And we’d like to get to the state:
FinalState = [on(b1, table), on(b2, b3), on(b3, b1), on(b4, b5), on(b5, b6),on(b6, table), on(b7, b4), on(b8, b7)]

