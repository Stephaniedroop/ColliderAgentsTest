/******************************************************************************/
/*** GW - collider with unobserved variables **********************************/
/******************************************************************************/
/* shift+opt+A */

/* CURRENT PROBLEMS / TO DO
- It took me 6 mins being slow; suggest pilot pay them for 10?
- Run prolific pilot, have about 40£ there
- Collect demogs
- Comprehension check - 4-6 radio buttons with binary Qs about things they may get wrong. Then don't worry about the attention check. 
- Get it on a loop using https://github.com/jspsych/jsPsych/discussions/2646 and https://www.jspsych.org/7.0/overview/timeline/#looping-timelines 

Experiment found at:
- https://eco.ppls.ed.ac.uk/~s0342840/collider/exp.html

*/

/******************************************************************************/
/*** Admin and setup  ******************************************************/
/******************************************************************************/

// generate subject ID

var subject_id = jsPsych.randomization.randomID(10);
console.log(subject_id);
// var prolific_id = jsPsych.data.getURLVariable("PROLIFIC_PID"); // uncomment these 3 then 3 below once it"s all working
// var study_id = jsPsych.data.getURLVariable("STUDY_ID");
// var session_id = jsPsych.data.getURLVariable("SESSION_ID");

jsPsych.data.addProperties({
  // prolific_id: prolific_id,
  // study_id: study_id,
  // session_id: session_id,
  subject_id: subject_id,
  //list: list,
});

function saveData(name, data_in) {
  var url = 'save_data.php';
  var data_to_send = { filename: name, filedata: data_in };
  fetch(url, {
    method: 'POST',
    body: JSON.stringify(data_to_send),
    headers: new Headers({
      'Content-Type': 'application/json',
    }),
  });
}

function saveDataLine(data) {
  //console.log(data);
  //choose the data we want to save
  var data_to_save = [
    data.subject_id,
    data.rt,
    data.time_elapsed,
    data.prolific_id,
    data.cb,
    data.study_id,
    data.session_id,
    data.prob0,
    data.prob1,
    data.prob2,
    data.prob3,
    data.scenario,
    data.A,
    data.B,
    data.E,
    data.S,
    data.answer,
    data.comments,
  ];
  //join these with commas and add a newline
  var line = data_to_save.join(',') + '\n';
  saveData(subject_id + '_collider.csv', line);
}

var write_headers = {
  type: 'call-function',
  func: function () {
    saveData(
      subject_id + '_collider.csv',
      'subject_id,rt,time_elapsed,prolific_id,cb,study_id,session_id,prob0,prob1,prob2,prob3,scenario,A,B,E,S,answer,comments\n',
    );
  },
};

/* var preload = {
  type: 'preload',
  images: [
    cookpics,
    jobpics,
    grouppics,
    'job1.png',
    'job2.png',
    'job3.png',
    'job4.png',
  ], // TO DO - MIGHT NEED NEW WAYS TO LIST FIGS IN ARRAYS
}; */

/******************************************************************************/
/*** Front and back matter, Instruction trials ****************************/
/******************************************************************************/

// Consent screen
var consent1 = {
  type: 'image-button-response',
  stimulus: 'PIS1.png',
  choices: ['Next'],
};

var consent2 = {
  type: 'image-button-response',
  stimulus: 'PIS2.png',
  choices: ['Yes, I consent to participate'],
};

var instructions = {
  type: 'instructions',
  pages: [
    'Welcome to the experiment. Click next to begin.',
    'In this experiment you will read some short story scenarios about different events. ',
    'There are three different scenarios: a job interview, a cookery show, and a university reading group. ',
    'Each scenario first has a random picture just for decoration and to set the scene. Like this: ' +
      '<br>' +
      '<img src="job2.png"></img>',
    'Each scenario has two main causes that can cause the effect,',
    'In the job interview, the causes are two skills, A and B, and the effect is getting offered the job.',
    'In the cookery show, the causes are two dishes, a main dish and a dessert, and the effect is winning.',
    'In the reading group, the causes are two people, a lecturer and a postdoc, and the effect is having a good discussion.',
    'Sometimes both causes need to happen for the effect to happen: ' +
      '<br>' +
      '<img src="111a.svg"></img>' +
      '<br>' +
      'like a lightswitch that needs both switches to be on.',
    'At other times, either event can cause the effect : ' +
      '<br>' +
      '<img src="101o.svg"></img>' +
      '<br>' +
      'like a lightswitch that can be switched on by either switch.',
    'It is more complicated though, because the events that correspond to the switch wires do not always <i>work</i>.',
    'In the lightswitch analogy, if the wires are faulty, the light may not come on, even if the switches are on.' +
      '<br>' +
      '<img src="110.svg"></img>' +
      '<br>' +
      'In this example, you can see the state of the switches and the light, but not the wires.',
    'You are told in general how often the switches are on, and how often the wires work.',
    'Then you are shown a specific event and asked to think about why that happened. ',
    'For each specific event you are asked about, you can see the state of the switches and of the lights, but not the wires.',
    'Likewise, in our stories, even when the causes happen, they do not always cause the effect. ', // example and diagram?
    'You will know in general how often things happen, but you will not have all the information about what happened <i>this time</i>.',
    'In each trial you are asked to imagine the event has just happened, and you are asked to think about that specific time only. ',
    'In the job interview, you know in <b>general</b> how often the skills are present and how often they are demonstrated.',
    'Each trial, you know whether the skills are present and whether they got the job, but you dont know whether the candidate demonstrated the skills.',
    'In the cookery show, you know how often the dishes are completed and how often they are impressive.',
    'Each trial, you know whether the dishes are completed and whether they won, but you dont know whether the dishes were impressive.',
    'In the reading group, you know how often the lecturer and postdoc attend and how often they talk about the paper. ',
    'Each trial, you know whether they attended and whether there was a good discussion, but you dont know whether they read the paper.',
    'You have to choose the best explanation for what happened. ',
    'You will do 12 of these trials. The scenarios and probabilities will vary. ',
    'That is all! Now do a short quiz to check your understanding.',
  ],
  show_clickable_nav: true,
};

var comp_check = {
  // but how do I say what answer is required?
  type: 'survey-html-form',
  preamble:
    "<p style='text-align:left'> Please answer a few questions about the task to check you know what to do.</p>",
  html: "<p style='text-align:left'>Each trial shows an event, preceded by some probabilities of how often the different causes happen. <br>  \
            <input required type='radio' name='Q0' value='yes'>Yes<br>\
            <input required type='radio' name='Q0' value='no'>No<br></p> \
            <p style='text-align:left'>There is a single correct explanation for each event. <br>  \
            <input required type='radio' name='correct' value='yes'>Yes<br>\
            <input required type='radio' name='correct' value='no'>No<br></p> \
        <p style='text-align:left'>How many trials do you have to do? <br> \
            <input required name='age' type='number'></p>",
  on_finish: function (data) {
    if (
      (data.response.Q0 == 'yes') &
      (data.response.correct == 'no') &
      (data.response.age == 12)
    ) {
      // add more questions
      data.correct = true;
    } else {
      data.correct = false;
    }
    console.log(data.correct);
  },
};

// Then get that on a loop if they fail the comprehension check SEE EXAMPLES IN THE LINKS ABOVE

var loop_instruction_timeline = {
  timeline: [instructions, comp_check],
  loop_function: function (data) {
    if (last_trial_correct) {
      return false;
    } else {
      return true;
    }
  },
};

// Debrief
var debrief = {
  type: 'instructions',
  pages: [
    'Thank you for taking part in this study. Click the button below to end the study and submit the data. You will then be returned to Prolific.</br>\
    <b>Do not click off the page; your data may not be saved. Only press the finish button below</b>.',
  ],
  show_clickable_nav: true,
  button_label_next: 'Finish',
};

var trialy = {
  type: 'html-button-response',
  prompt: '<p>Press a key! This is a test</p>',
  choices: ['Next'],
  stimulus:
    '<p style="display:inline-block;color:red;font-size:40px;">testy<img src="collider100.drawio.svg">testing</p>',
};
/* on_load: () => {
    const html = `<div id="black-box" <p style='color:red; position:absolute; top:200; right:200; width:200px; height:100px;'> Testing </div>`;
    document.querySelector('body').insertAdjacentHTML('beforeend', html);
  },
  on_finish: () => {
    document.querySelector('#black-box').remove();
  }, */
/* trial_duration: jsPsych.timelineVariable('duration'),
  data: {
    fixation_duration: jsPsych.timelineVariable('fixation_duration'),
    trial_duration: jsPsych.timelineVariable('duration'),
    image1: jsPsych.timelineVariable('image1'),
    image2: jsPsych.timelineVariable('image2'),
  }, */

/******************************************************************************/
/*** Main part - probs and worlds - standard static to be used in all conditions ****/
/******************************************************************************/

/* PROBS 
(This now done inside each trial, but could be done here if we wanted to keep them static)

First: 1 5 8 5: A rare, B usually happens, but they both only work half the time
Second: 5 1 5 8: A and B both happen half the time, but A only rarely works, and B usually works
Third: 1, 7, 8, 5: all random*/

// Counterbalance the order effect of A and B vars. This is randomly sampled inside each trial
var counterbalances = [1, 2];

// Other global vars, modified later during each trial to contain the html contents and inserted keys which varies for each trial
var scen = '';
var cond = '';
var pic = '';
var prob = [];
var cb = [];

/* Now a series of objects for all 'worlds'. A 'world' here is a setting consisting of:
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
  maxAns: [1, 2, 3, 4, 5, 6, 7, 8],
  medAns: [1, 3, 5, 7],
  possAns: [1, 5],
  diag: '000.svg',
};

const c2 = {
  A: 0,
  B: 1,
  E: 0,
  S: 0,
  maxAns: [1, 2, 3, 4, 5, 6, 7, 8],
  //medAns: [1, 3, 6, 7], not clear what actually goes in this category - intended to be all the ones that are compatible?
  possAns: [1, 7],
  diag: '010.svg',
};

const c3 = {
  A: 1,
  B: 0,
  E: 0,
  S: 0,
  maxAns: [1, 2, 3, 4, 5, 6, 7, 8],
  //medAns: [0, 3, 5, 7],
  possAns: [3, 5],
  diag: '100.svg',
};

const c4 = {
  A: 1,
  B: 1,
  E: 0,
  S: 0,
  maxAns: [1, 2, 3, 4, 5, 6, 7, 8],
  possAns: [3, 7],
  diag: '110.svg',
};

const c5 = {
  A: 1,
  B: 1,
  E: 1,
  S: 0,
  maxAns: [1, 2, 3, 4, 5, 6, 7, 8],
  possAns: [0, 2, 4, 6],
  diag: '111.svg',
};

// DISJUNCTIVE
const d1 = {
  A: 0,
  B: 0,
  E: 0,
  S: 1,
  maxAns: [1, 2, 3, 4, 5, 6, 7, 8],
  possAns: [1, 5],
  diag: '000.svg',
};

const d2 = {
  A: 0,
  B: 1,
  E: 0,
  S: 1,
  maxAns: [1, 2, 3, 4, 5, 6, 7, 8],
  possAns: [1, 7],
  diag: '010.svg',
};

const d3 = {
  A: 0,
  B: 1,
  E: 1,
  S: 1,
  maxAns: [1, 2, 3, 4, 5, 6, 7, 8],
  possAns: [4, 6],
  diag: '011.svg',
};

const d4 = {
  A: 1,
  B: 0,
  E: 0,
  S: 1,
  maxAns: [1, 2, 3, 4, 5, 6, 7, 8],
  possAns: [3, 5],
  diag: '100.svg',
};

const d5 = {
  A: 1,
  B: 0,
  E: 1,
  S: 1,
  maxAns: [1, 2, 3, 4, 5, 6, 7, 8],
  possAns: [0, 2],
  diag: '101.svg',
};

const d6 = {
  A: 1,
  B: 1,
  E: 0,
  S: 1,
  maxAns: [1, 2, 3, 4, 5, 6, 7, 8],
  possAns: [3, 7],
  diag: '110.svg',
};

const d7 = {
  A: 1,
  B: 1,
  E: 1,
  S: 1,
  maxAns: [1, 2, 3, 4, 5, 6, 7, 8],
  possAns: [0, 2, 4, 6],
  diag: '111.svg',
};

// Array of all the worlds to be shuffled to give the experiment flow for each participant
var all_worlds = [c1, c2, c3, c4, c5, d1, d2, d3, d4, d5, d6, d7];
// Shuffle them, once for each participant, who will see all once each.
var all_worlds_shuffled = jsPsych.randomization.shuffle(all_worlds);

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
    '<p>... a job interview.</p> <p>Having the skills on paper is not enough to be offered the job: the candidate also has to demonstrate their skill in person to the interview panel.</p>',
  name: 'job',
  Atext: ['did not have skill A', 'had skill A'],
  Btext: ['did not have skill B', 'had skill B'],
  Etext: ['not offered a job', 'offered a job'],
  scenOpt1: ['both skills', 'either skill'],
  fig: ['joband.svg', 'jobor.svg'],
};

const cook = {
  name: 'cook',
  description:
    "<p>... a cookery show on television, where chefs have to prepare a main dish and a dessert under time pressure.</p>\
    <p>The show panel judges each of the two dishes, and decides whether it is <i>successful</i> (tasty, well-made, etc.) or not.</p>\
  <p>However, the panel will only judge a dish if it is completed on time.</p>\
   <p>Some say the show's popularity is due to the panel's notoriously fickle tastes...</p> ",
  Atext: ['did not complete the main dish', 'completed the main dish'],
  Btext: ['did not complete the dessert', 'completed the dessert'],
  Etext: ['did not progress to the next stage', 'progressed to the next stage'],
  scenOpt1: [
    'both the main dish and dessert are both completed and are both impressive',
    'either the main dish or dessert is completed and impressive',
  ],
  fig: ['cookand.svg', 'cookor.svg'],
};

const group = {
  name: 'group',
  description:
    '<p>... a small university reading group of students and their advisors.</p>\
    <p>The students always attend, but the lecturer and postdoc only sometimes attend.</p> \
    <p>Even when they attend, they do not always talk about the allotted paper.</p>',
  Atext: ['did not attend', 'attended'],
  Btext: ['did not attend', 'attended'],
  Etext: ['not a good discussion', 'a good discussion'],
  scenOpt1: [
    'both the lecturer and the postdoc ',
    'either the lecturer or postdoc ',
  ],
  fig: ['groupand.svg', 'groupor.svg'],
};

// Now altogether to be randomised in the trial generation stage
var all_three = [job, cook, group];

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
  { stimulus: 'cook1.png' },
  { stimulus: 'cook2.png' },
  { stimulus: 'cook3.png' },
  { stimulus: 'cook4.png' },
];

var jobpics = [
  { stimulus: 'job1.png' },
  { stimulus: 'job2.png' },
  { stimulus: 'job3.png' },
  { stimulus: 'job4.png' },
];

var grouppics = [
  { stimulus: 'group1.png' },
  { stimulus: 'group2.png' },
  { stimulus: 'group3.png' },
];

/******************************************************************************/
/*** The four functions in order: pic, scen, cond, ans  ************************/
/******************************************************************************/

// FUNCTION 1 to show a picture to set the scene. Screen 1 of 5
function make_pic(scenario) {
  if (scenario == job) {
    var pics = jobpics;
  } else if (scenario == cook) {
    var pics = cookpics;
  } else if (scenario == group) {
    var pics = grouppics;
  }
  pic = jsPsych.randomization.shuffle(pics)[0].stimulus;
  var trial = {
    type: 'image-button-response',
    stimulus: pic,
    stimulus_height: 300,
    maintain_aspect_ratio: true,
    prompt: '<b>The situation is ...</b>' + scenario.description + '',
    choices: ['Next'],
  };
  return trial;
}

// FUNCTION 2: SCEN1of2: Make scenario for each trial. (ie screen 2 of 5) -- setting the specific situation part 1
function make_scenario1(scenario, world) {
  if (scenario == job) {
    scen =
      '<p style="text-align:left">Skill <b>A</b> and Skill <b>B</b> are both relevant for this position,\
     and the company is interested in candidates whose application shows <b><u style = "color:#800080"> ' +
      scenario.scenOpt1[world.S] +
      "</u></b>.</p>\
     <p style='text-align:left'>Of applications received, <b><u style = 'color:#800080'>" +
      prob[0] +
      "</u></b> of applicants are deemed to have Skill <b>A</b> and <b><u style = 'color:#800080'> " +
      prob[2] +
      " </u></b> have Skill <b>B</b>.<br>\
      <p style='text-align:left'>Skill <b>A</b> tends to be demonstrated <b><u style = 'color:#800080'>" +
      prob[1] +
      " </u></b> of the time it is present. <br>\
       Skill <b>B</b> tends to be demonstrated <b><u style = 'color:#800080'> " +
      prob[3] +
      ' </u></b> of the time it is present.</p>';
  } else if (scenario == cook) {
    scen =
      '<p style="text-align:left">On average throughout the shows history, chefs tend to complete <b><u style = "color:#800080">' +
      prob[0] +
      " </u></b> of main dishes and <b><u style = 'color:#800080'>" +
      prob[2] +
      " </u></b> of desserts within the allotted time.</p>\
      <p style='text-align:left'>The panel is impressed by <b><u style = 'color:#800080'>" +
      prob[1] +
      " </u></b> of the completed main dishes and <b><u style = 'color:#800080'>" +
      prob[3] +
      " </u></b> of the completed desserts.<br>\
    <p style='text-align:left'>The chef wins the task and can progress to the next round if <b><u style = 'color:#800080'>" +
      scenario.scenOpt1[world.S] +
      '</u></b>.</p>';
  } else if (scenario == group) {
    scen =
      '<p style="text-align:left">The <b>lecturer</b> attends <b><u style = "color:#800080">' +
      prob[0] +
      " </u></b> of the time and the <b>postdoc</b> attends <b><u style = 'color:#800080'>" +
      prob[2] +
      " </u></b> of the time.</p>\
     <p style='text-align:left'>The lecturer talks about the paper <b><u style = 'color:#800080'>" +
      prob[1] +
      " </u></b> of the times they attend, and the postdoc talks about the paper <b><u style = 'color:#800080'>" +
      prob[3] +
      " </u></b> of the times they attend. <br>\
     <p style='text-align:left'>A good discussion happens when <b><u style = 'color:#800080'> " +
      scenario.scenOpt1[world.S] +
      ' </u></b> attend and talk about the allocated paper.</p>';
  }
  var trial = {
    type: 'html-button-response-custom',
    stimulus: scen,
    //prompt: '<b>Simplified general situation</b>',
    choices: ['Next'],
    on_finish: function (data) {
      data.cb = cb;
      data.prob0 = prob[0];
      data.prob1 = prob[1];
      data.prob2 = prob[2];
      data.prob3 = prob[3];
      data.scenario = scenario.name; //tried before JSON.stringify(scenario) but no need for whole object
      data.A = world.A;
      data.B = world.B;
      data.E = world.E;
      data.S = world.S;
      saveDataLine(data);
    },
  };
  return trial;
}

// FUNCTION 3: SCEN2of2: Make scenario for each trial. (ie screen 3 of 5) -- setting the specific situation part 2
function make_scenario2(scenario, world) {
  if (scenario == job) {
    scen2 =
      '<p style="display:inline-block;color:red;font-size:20px;">A is present ' +
      prob[0] +
      ' and works ' +
      prob[1] +
      ' <img src=' +
      scenario.fig[world.S] +
      '> B is present ' +
      prob[2] +
      ' and works ' +
      prob[3] +
      ' </p>';
  } else if (scenario == cook) {
    scen2 =
      '<p style="display:inline-block;color:red;font-size:20px;"> Main dish completed ' +
      prob[0] +
      ' and impressive ' +
      prob[1] +
      ' <img src=' +
      scenario.fig[world.S] +
      '> Dessert completed ' +
      prob[2] +
      ' and impressive ' +
      prob[3] +
      ' </p>';
  } else if (scenario == group) {
    scen2 =
      '<p style="display:inline-block;color:red;font-size:20px;"> Lecturer attends' +
      prob[0] +
      ' and talks ' +
      prob[1] +
      ' <img src=' +
      scenario.fig[world.S] +
      '> Postdoc attends ' +
      prob[2] +
      ' and talks ' +
      prob[3] +
      ' </p>';
  }
  var trial = {
    type: 'html-button-response-custom',
    stimulus: scen2,
    prompt: '<b>Simplified general situation</b>',
    choices: ['Next'],
    on_finish: function (data) {
      // prob don't need to save the data twice?
      data.cb = cb;
      data.prob0 = prob[0];
      data.prob1 = prob[1];
      data.prob2 = prob[2];
      data.prob3 = prob[3];
      data.scenario = scenario.name; //tried before JSON.stringify(scenario) but no need for whole object
      data.A = world.A;
      data.B = world.B;
      data.E = world.E;
      data.S = world.S;
      saveDataLine(data);
    },
  };
  return trial;
}

// FUNCTION 4: COND: Make condition for each trial. (ie screen 4 of 5) -- what happened THIS TIME
function make_condition(condition, world) {
  if (condition == job) {
    cond =
      '<img src=' +
      world.diag +
      "> </b> <p style='text-align:left'>On this occasion, the candidate <b><u style = 'color:#800080'>" +
      condition.Atext[world.A] +
      " </u></b> and <b><u style = 'color:#800080'>" +
      condition.Btext[world.B] +
      " </u></b> and was <b><u style = 'color:#800080'>" +
      condition.Etext[world.E] +
      '.</u></b> </p>';
  } else if (condition == cook) {
    cond =
      '<img src=' +
      world.diag +
      "> </b> <p style='text-align:left'>On this occasion, the chef <b><u style = 'color:#800080'>" +
      condition.Atext[world.A] +
      " </u></b> and <b><u style = 'color:#800080'>" +
      condition.Btext[world.B] +
      " </u></b> and they <b><u style = 'color:#800080'>" +
      condition.Etext[world.E] +
      '.</u></b></p>';
  } else if (condition == group) {
    cond =
      '<img src=' +
      world.diag +
      "> </b> <p style='text-align:left'>On this occasion, the lecturer <b><u style = 'color:#800080'>" +
      condition.Atext[world.A] +
      " </u></b> and the postdoc <b><u style = 'color:#800080'>" +
      condition.Btext[world.B] +
      " </u></b> and there was <b><u style = 'color:#800080'>" +
      condition.Etext[world.E] +
      '.</u></b></p>';
  }
  var trial = {
    type: 'html-button-response',
    stimulus: cond,
    choices: ['Next'],
  };
  return trial;
}

/******************************************************************************/
/*** Possible answers *******************************************************/
/******************************************************************************/
// These are used in function 5, below
var job_ans = [
  'The candidate had skill A',
  'The candidate did not have skill A',
  'The candidate demonstrated skill A',
  'The candidate did not demonstrate skill A',
  'The candidate had skill B',
  'The candidate did not have skill B',
  'The candidate demonstrated skill B',
  'The candidate did not demonstrate skill B',
];

var cook_ans = [
  'The chef completed the main dish',
  'The chef did not complete the main dish',
  'The main dish impressed the panel',
  'The main dish did not impress the panel',
  'The chef completed the dessert',
  'The chef did not complete the dessert',
  'The dessert impressed the panel',
  'The dessert did not impress the panel',
];

var group_ans = [
  'The lecturer attended',
  'The lecturer did not attend',
  'The lecturer talked about the paper',
  'The lecturer did not talk about the paper',
  'The postdoc attended',
  'The postdoc did not attend',
  'The postdoc talked about the paper',
  'The postdoc did not talk about the paper',
];

/******************************************************************************/
/*** Collect answers *******************************************************/
/******************************************************************************/

// FUNCTION 5: Makes radio buttons for each permissable answer.
function make_answers(scenario, world) {
  if (scenario == job) {
    var ans = job_ans;
  } else if (scenario == cook) {
    var ans = cook_ans;
  } else if (scenario == group) {
    var ans = group_ans;
  }
  //Next chunk is to make sure we only show the radio buttons that are consistent with the actual causation.
  //For pilot to show the whole array of 8 answers, replace 3x poss.Ans with maxAns
  var possAns = world.possAns; // the indices of possible answers for this world were set in the world object
  anstext = [];
  for (var i = 0; i < possAns.length; i++) {
    anstext[i] = ans[possAns[i]]; // take the text for those permissable indices
  }
  actual_ans = []; // Make an array for permissble answers
  //actual_ans.push(cond);
  for (var j = 0; j < anstext.length; j++) {
    // then make a radio button for each of those permissable answers
    // actual_ans[j] =
    //   '<input type="radio" name="q1" required value=' +
    //   JSON.stringify(anstext[j]) + // where the text itself is stored as the value to save for later as our data
    //   ' >' +
    //   anstext[j] + // and the text is also displayed against the buttons
    //   '<br>';
    actual_ans.push(
      '<input type="radio" name="q1" required value=' +
        JSON.stringify(anstext[j]) + // where the text itself is stored as the value to save for later as our data
        ' >' +
        anstext[j] + // and the text is also displayed against the buttons
        '<br>',
    );
  }
  actual_ans.push(
    /* ' <br> <br> <img src=' + pic + ' width="30%" height="30%" </img>', */ // this was to show the pic again but it's not needed
    "<p style='text-align:left'> <br> <br> <b>Reminder of what happened:</b>  ",
    ' ' + scen2 + '</p>',
    ' ' + cond + '</p>',
  );

  var trial = {
    type: 'survey-html-form',
    preamble: '<b>What is the best explanation for what happened?</b> <br>',
    html: actual_ans.join(''), // The .join('') is important to removes commas that are inserted by default when parts of an array are joined.
    post_trial_gap: 500,
    on_finish: function (data) {
      data.answer = data.response.q1;
      saveDataLine(data);
    },
  };
  return trial;
}

/******************************************************************************/
/*** Situation timelines *******************************************************/
/******************************************************************************/

// Make a timeline - function 6 - puts 1-5 together

function make_timeline() {
  var timelinechunk = [];
  for (i = 0; i < all_worlds_shuffled.length; i++) {
    var all_three_shuffled = jsPsych.randomization.shuffle(all_three);
    var scenario = all_three_shuffled[0];
    cb = jsPsych.randomization.shuffle(counterbalances)[0];
    if (cb == 1) {
      var probs = [
        ['10%', '50%', '80%', '50%'],
        ['50%', '10%', '50%', '80%'],
        ['10%', '70%', '80%', '50%'],
      ];
    } else if (cb == 2) {
      var probs = [
        ['80%', '50%', '10%', '50%'],
        ['50%', '80%', '50%', '10%'],
        ['80%', '50%', '10%', '70%'],
      ];
    }
    prob = jsPsych.randomization.shuffle(probs)[0];
    var piece0 = make_pic(scenario);
    timelinechunk.push(piece0);
    var piece1 = make_scenario1(scenario, all_worlds_shuffled[i]);
    timelinechunk.push(piece1);
    var piece2 = make_scenario2(scenario, all_worlds_shuffled[i]);
    timelinechunk.push(piece2);
    var piece3 = make_condition(scenario, all_worlds_shuffled[i]);
    timelinechunk.push(piece3);
    var piece4 = make_answers(scenario, all_worlds_shuffled[i]);
    timelinechunk.push(piece4);
  }
  return timelinechunk;
}

/******************************************************************************/
/*** Collect demographics *******************************************************/
/******************************************************************************/

var feedback_form = {
  type: 'survey-html-form',
  preamble:
    "<p style='text-align:left'> Do you have any comments or feedback on our experiment?</p>",
  html: "<p style='text-align:left'>This will help us make it better. Thankyou!<br> \
            <textarea name='comments'rows='10' cols='60'></textarea></p>",
  button_label: 'Submit',
  on_finish: function (data) {
    data.comments = data.response.comments;
    saveDataLine(data);
  },
};

/******************************************************************************/
/*** Build the timeline *******************************************************/
/******************************************************************************/

var timelinechunk = make_timeline();

/******************************************************************************/
var full_timeline = [].concat(
  //preload,
  write_headers,
  comp_check,
  instructions,
  /* trialy,
  consent1,
  consent2,
  timelinechunk, */
  feedback_form,
  debrief,
);

jsPsych.init({
  timeline: full_timeline,
  show_progress_bar: true,
  on_finish: function () {
    jsPsych.endExperiment();
    // window.location = "https://app.prolific.co/submissions/complete?cc=223125C8";
  },
});
