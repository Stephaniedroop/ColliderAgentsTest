/******************************************************************************/
/*** COLLIDER DATA STRUCTURES  ************************************************/
/******************************************************************************/

/* Static script to define:
1. Worlds (collider settings)
2. Conditions (vignettes to translate collider settings to real life scenarios)  */

/* A series of objects for all 'worlds'. A 'world' here is a setting consisting of:
- the two observed variables of the collider, A and B, with their setting 0 for absent and 1 for present;
- the variable E for effect or outcome, and its setting of 0 for didn't happen and 1 for did happen
- a variable S for the logical function of the collider, where 
-- 0=conjunctive (ie both A and B are needed for E) and 
-- 1=disjunctive (ie one of A or B is needed for E).
- the possible answers, which are the indices of the answers that are consistent with that world in the arrays of all answers given below in the 
The 5 conjunctive worlds and the 7 disjunctive worlds are all the possible settings allowed by those structural equations
*/

// CONJUNCTIVE
const c1 = {
  A: 0,
  B: 0,
  E: 0,
  S: 0,
  trialtype: 'c1',
  maxAns: [0, 1, 2, 3, 4, 5, 6, 7],
  possAns: [1, 5],
  diag: '000.svg',
  job: 'joband000.svg',
  cook: 'cookand000.png',
  group: 'groupand000.png',
};

const c2 = {
  A: 0,
  B: 1,
  E: 0,
  S: 0,
  trialtype: 'c2',
  maxAns: [0, 1, 2, 3, 4, 5, 6, 7],
  possAns: [1, 7],
  diag: '010.svg',
  job: 'joband010.svg',
  cook: 'cookand010.png',
  group: 'groupand010.png',
};

const c3 = {
  A: 1,
  B: 0,
  E: 0,
  S: 0,
  trialtype: 'c3',
  maxAns: [0, 1, 2, 3, 4, 5, 6, 7],
  possAns: [3, 5],
  diag: '100.svg',
  job: 'joband100.svg',
  cook: 'cookand100.png',
  group: 'groupand100.png',
};

const c4 = {
  A: 1,
  B: 1,
  E: 0,
  S: 0,
  trialtype: 'c4',
  maxAns: [0, 1, 2, 3, 4, 5, 6, 7],
  possAns: [3, 7],
  diag: '110.svg',
  job: 'joband110.svg',
  cook: 'cookand110.png',
  group: 'groupand110.png',
};

const c5 = {
  A: 1,
  B: 1,
  E: 1,
  S: 0,
  trialtype: 'c5',
  maxAns: [0, 1, 2, 3, 4, 5, 6, 7],
  possAns: [0, 2, 4, 6],
  diag: '111.svg',
  job: 'joband111.svg',
  cook: 'cookand111.png',
  group: 'groupand111.png',
};

// DISJUNCTIVE
const d1 = {
  A: 0,
  B: 0,
  E: 0,
  S: 1,
  trialtype: 'd1',
  maxAns: [0, 1, 2, 3, 4, 5, 6, 7],
  possAns: [1, 5],
  diag: '000.svg',
  job: 'jobor000.svg',
  cook: 'cookor000.png',
  group: 'groupor000.png',
};

const d2 = {
  A: 0,
  B: 1,
  E: 0,
  S: 1,
  trialtype: 'd2',
  maxAns: [0, 1, 2, 3, 4, 5, 6, 7],
  possAns: [1, 7],
  diag: '010.svg',
  job: 'jobor010.svg',
  cook: 'cookor010.png',
  group: 'groupor010.png',
};

const d3 = {
  A: 0,
  B: 1,
  E: 1,
  S: 1,
  trialtype: 'd3',
  maxAns: [0, 1, 2, 3, 4, 5, 6, 7],
  possAns: [4, 6],
  diag: '011.svg',
  job: 'jobor011.svg',
  cook: 'cookor011.png',
  group: 'groupor011.png',
};

const d4 = {
  A: 1,
  B: 0,
  E: 0,
  S: 1,
  trialtype: 'd4',
  maxAns: [0, 1, 2, 3, 4, 5, 6, 7],
  possAns: [3, 5],
  diag: '100.svg',
  job: 'jobor100.svg',
  cook: 'cookor100.png',
  group: 'groupor100.png',
};

const d5 = {
  A: 1,
  B: 0,
  E: 1,
  S: 1,
  trialtype: 'd5',
  maxAns: [0, 1, 2, 3, 4, 5, 6, 7],
  possAns: [0, 2],
  diag: '101.svg',
  job: 'jobor101.svg',
  cook: 'cookor101.png',
  group: 'groupor101.png',
};

const d6 = {
  A: 1,
  B: 1,
  E: 0,
  S: 1,
  trialtype: 'd6',
  maxAns: [0, 1, 2, 3, 4, 5, 6, 7],
  possAns: [3, 7],
  diag: '110.svg',
  job: 'jobor110.svg',
  cook: 'cookor110.png',
  group: 'groupor110.png',
};

const d7 = {
  A: 1,
  B: 1,
  E: 1,
  S: 1,
  trialtype: 'd7',
  maxAns: [0, 1, 2, 3, 4, 5, 6, 7],
  possAns: [0, 2, 4, 6],
  diag: '111.svg',
  job: 'jobor111.svg',
  cook: 'cookor111.png',
  group: 'groupor111.png',
};

/* Now a series of objects to define the three scenarios, to translate the collider to 'real life' through verbal vignettes. 
The cookery show was loosely based on Zultan and Lagnado 2012, and the others were made up by experimenters.
The arrays are:
- Atext: the text for the absence of A or its presence (0, 1)
- Ditto B
- Ditto E
scenOpt1: the text for the setting of the collider, where 0=conjunctive and 1=disjunctive

A random one is picked for each trial */

const job = {
  description:
    '<p>... a work meeting, where a company is trying to sell a product to a client.</p> \
    <p>The presenter introduces some features in a talk, and then opens files to demonstrate. </p> \
    <p>However, the laptop files are sometimes randomly corrupted and fail to open! </p> \
    <p>The presenter does not know in advance which files are corrupted.</p>',
  name: 'job',
  Atext: ['did not present feature A', 'presented feature A'],
  Btext: ['did not present feature B', 'presented feature B'],
  Etext: ['did not accept', 'accepted'],
  scenOpt1: ['both features', 'either feature'],
  fig: ['joband.svg', 'jobor.svg'],
};

const cook = {
  name: 'cook',
  description:
    '<p>... a person walking down a street full of cafes, looking for a place to eat.</p>\
    <p>They have already decided what they want to eat: a main dish, a dessert, or both.</p>\
    <p>They read the menus, and decide whether to enter and eat something.</p>\
   <p>If the food is on the menu and they want to eat it, they will enter the cafe.</p> ',
  Atext: ['main dish was not on the menu', 'main dish was on the menu'],
  Btext: ['dessert was not on the menu', 'dessert was on the menu'],
  Etext: ['did not enter the cafe', 'entered the cafe'],
  scenOpt1: [
    'the main dish and dessert are both',
    'either one of the main dish or dessert is',
  ],
  fig: ['cookand.svg', 'cookor.svg'],
};

const group = {
  name: 'group',
  description:
    '<p>... an afternoon university seminar class.</p>\
    <p>The class is compulsory, but two particular students only sometimes attend.</p> \
    <p>These two students are intelligent, passionate, articulate and well-read. </p> \
    <p>However, even when they attend, they are only in the mood to talk if they have had a good morning! </p> \
    <p>(Their morning and hence their mood does not affect how likely they are to turn up; a bad morning just makes them quiet.)</p>\
    <p>But if they attend and had a good morning, there will always be a good discussion.</p>',
  Atext: ['did not attend', 'attended'],
  Btext: ['did not attend', 'attended'],
  Etext: ['not a good discussion', 'a good discussion'],
  scenOpt1: ['both students attend', 'either student attends'],
  fig: ['groupand.svg', 'groupor.svg'],
};

// OTHER DATA FOR STRUCTURE
// Now altogether to be randomised in the trial generation stage
var all_three = [job, cook, group];
// Counterbalance the order effect of A and B vars. This is randomly sampled inside each trial - NO - ATM IT IS DONE ONCE FOR EACH PPT
var counterbalances = [0, 1];
// Array of all the worlds to be shuffled to give the experiment flow for each participant
var all_worlds = [c1, c2, c3, c4, c5, d1, d2, d3, d4, d5, d6, d7];

var probs = [
  [
    ['10%', '50%', '80%', '50%'],
    ['50%', '10%', '50%', '80%'],
    ['10%', '70%', '80%', '50%'],
  ],
  [
    ['80%', '50%', '10%', '50%'],
    ['50%', '80%', '50%', '10%'],
    ['80%', '50%', '10%', '70%'],
  ],
];

/******************************************************************************/
/*** Generate trials  *******************************************************/
/******************************************************************************/

/* A 'trial' is a set of 4 screens. Each of the 12 worlds gets a set of 4 screens. 
A trial is made of a 
- 'pic' (a picture to set the scene with some static text about the scenario)
- 'scen' (short for setting the scene, like a story. It sets the probabilities and is dynamic) 
- 'cond' (short for condition, containing the relevant values of the variables A, B and E for what happened this time)
- 'ans' - (short for answers; radio buttons relevant and consistent to that scenario. 
  Can be either a maximal version with all, or a minimal only the ones consustent with actual causation)

Then they are put together again so ppts can see all the relevant info together on one screen
*/

/******************************************************************************/
/*** Some test code for pics   ************************/
/******************************************************************************/

var cookpics = [
  { stimulus: 'cook1.jpeg' },
  { stimulus: 'cook2.jpeg' },
  { stimulus: 'cook3.jpeg' },
  { stimulus: 'cook4.jpeg' },
  { stimulus: 'cook5.jpeg' },
];

var jobpics = [
  { stimulus: 'job1.jpeg' },
  { stimulus: 'job2.jpeg' },
  { stimulus: 'job3.jpeg' },
  { stimulus: 'job4.jpeg' },
  { stimulus: 'job5.jpeg' },
  { stimulus: 'job6.jpeg' },
  { stimulus: 'job7.jpeg' },
];

var grouppics = [
  { stimulus: 'group1.jpeg' },
  { stimulus: 'group2.jpeg' },
  { stimulus: 'group3.jpeg' },
  { stimulus: 'group4.jpeg' },
  { stimulus: 'group5.jpeg' },
];
