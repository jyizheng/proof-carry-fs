%% For the procap test

grp: sort.
dg@mygroup: term grp.

level: sort.

secret: term level.
topsecret: term level.
confidential: term level.

below: term level -> term level -> constraint.

member: term principal -> term grp -> state.



%% For the term test

master: term any -> term any -> term any -> term any -> term int.

