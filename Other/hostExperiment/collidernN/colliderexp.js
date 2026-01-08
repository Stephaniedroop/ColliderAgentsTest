/******************************************************************************/
/*** GW - collider with unobserved variables **********************************/
/******************************************************************************/
/* shift+opt+A */

/* THIS IS A RERUN IN JUNE 2025. 
(Original was in July 2024)

New location:
- https://eco.ppls.ed.ac.uk/~s0342840/collidern/collidertop.html

Original experiment found at:
- https://eco.ppls.ed.ac.uk/~s0342840/collider/collidertop.html

*/

/******************************************************************************/
/*** Admin and setup  ******************************************************/
/******************************************************************************/

// Resaving data from colliderdata.js into JSON strings to use in R too - not strictly part of this experiment
/* const jasonedworlds = {
  c1: c1,
  c2: c2,
  c3: c3,
  c4: c4,
  c5: c5,
  d1: d1,
  d2: d2,
  d3: d3,
  d4: d4,
  d5: d5,
  d6: d6,
  d7: d7,
};
const jasonedworldsstring = JSON.stringify(jasonedworlds);
console.log(jasonedworldsstring);

const jasonedconds = { cook: cook, job: job, group: group };
const jasonedcondsstring = JSON.stringify(jasonedconds);
console.log(jasonedcondsstring); */

// Empry objects for static world and condition data
var tmpconds = {};

fetch('./conds.json')
  .then((response) => response.json())
  .then((json) => (tmpconds = json)); // this name tmp will change

var tmpworlds = {};

fetch('./worlds.json')
  .then((response) => response.json())
  .then((json) => (tmpworlds = json));

// generate subject ID

var subject_id = jsPsych.randomization.randomID(10);
//console.log(subject_id);
var prolific_id = jsPsych.data.getURLVariable('PROLIFIC_PID'); // uncomment these 3 then 3 below once it"s all working
var study_id = jsPsych.data.getURLVariable('STUDY_ID');
var session_id = jsPsych.data.getURLVariable('SESSION_ID');

jsPsych.data.addProperties({
  prolific_id: prolific_id,
  study_id: study_id,
  session_id: session_id,
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
    data.rowtype,
    data.trialtype,
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
      'subject_id,rt,time_elapsed,prolific_id,cb,study_id,session_id,prob0,prob1,prob2,prob3,scenario,A,B,E,S,answer,comments,rowtype,trialtype\n',
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

// Part 1 of a 3-part mini timeline to check understanding of the task. Gives instructions in a series of click through screens
var instructions = {
  type: 'instructions',
  pages: [
    'Welcome to the experiment. Click next to begin.',
    'First we need to warn you, this experiment is complex. You need to think hard and the payment includes time for thinking.',
    'You do not need to do maths, but you do need to think about percentages.',
    'For example, if something happens 50% of the time or with 50% probability, <br> then it happens half the time and is equally likely to happen or not happen.',
    'If something happens 10% of the time then it happens in general once in every ten situations, so not often.',
    'And if something happens 90% of the time then it happens quite frequently.',
    'In this experiment you will read some short story scenarios about different events, <br> and then choose the best explanation for what happened. ',
    'There are three different scenarios: a work meeting, a person thinking about eating in a cafe, and a university class discussion. ',
    'Each trial has an AI-generated picture to set the scene and (hopefully) stop you getting too bored. Like this: ' +
      '<br>' +
      '<img src="group2.jpeg" style="width:50%;"> </img>',
    '(But the picture is not part of the experiment and should not affect your understanding or answers). ',
    'Then you will read about a scenario. Each scenario has two observable causes and an outcome.',
    'In the work meeting, the observable causes are two features: A and B, <br> and the outcome is the client accepting the product.',
    'In the street cafe, the observable causes are two food items which may be on the menu, <br> and the outcome is the person entering the cafe to eat.',
    'In the university class, the observable causes are two particular students attending, <br> and the outcome is a good discussion.',

    'Now you will see some abstract pictures of the observable situation, where a green circle means a cause is present/ON, <br> and a grey circle means it is absent/OFF. ',
    'Like this!' + '<br>' + '<img src="illustration.png"></img>' + '<br>',
    'Sometimes both causes need to happen for the effect to happen: ' +
      '<br>' +
      '<img src="111a.svg"></img>' +
      '<br>' +
      'like a machine or light that needs two power sources in order to be on. It will be marked with AND.',
    'At other times, either event can cause the effect: ' +
      '<br>' +
      '<img src="101o.svg"></img>' +
      '<br>' +
      'like a machine or light that can be switched on by either of two power sources. It will be marked with OR.',
    'But there is a complication! There are also other events or causes you cannot see, <br> paired with the observable causes, which enable them to work.',
    'With the light analogy, it is like if the wires are faulty, the light may not come on, even if the switches are on.' +
      '<br>' +
      '<img src="110q.png"></img>' +
      '<br>' +
      'In this example, you can see the state of the switches and the light, but not the wires. <br> The switches were on, but the light did not come on.',
    'Likewise, in our stories, even when the observable causes happen, the effect does not always happen. ', // example and diagram?
    'Each cause is paired with something you cannot see, which can affect the outcome.',
    'In the work meeting, files are randomly corrupted and fail to open.',
    'In the cafe, the person does not want to eat certain foods.',
    'In the university group, the students may be too tired or grumpy to talk, <br> i.e. they need to have had a good morning to be engaged.',
    'Even if our examples seem a little contrived, you do not need to use your own knowledge of outside events.',
    'You will be told in general how often things happen, but you will not have all the information about what happened in each trial.',
    'In the work meeting, you will be told in <b>general</b> how often the company presents each feature <br> and how often files tend to be corrupted...',
    'Each trial, you know what they presented and whether they sold the product this time, <br> BUT you dont know what files opened or were corrupted.',
    'In the cafe, you will be told in <b>general</b> how often the dishes are on the menu <br> and how often the person wants to eat them...',
    'Each trial, you know whether the dishes were on the menu and whether the person entered the cafe this time, <br> BUT you dont know what the person wanted to eat, if anything.',
    'In the university class, you will be told in <b>general</b> how often each student attends <br> and how often they feel chatty because they had a good morning... ',
    'Each trial, you know who attended and whether there was a good discussion this time, <br> BUT you dont know whether the students had had a good morning.',
    'You have to choose the best explanation for what happened. ',
    'There is no right answer and we are not expecting you to prefer any type of answer more than others.',
    'On some trials you may find several answers possible but we want you to pick the best one, <br> the one you would pick if there can only be one.',
    'You will do 12 of these trials. The scenarios and probabilities will vary. ',
    'That is all! Now do a short quiz to check your understanding.',
  ],
  show_clickable_nav: true,
};

// Comprehension check questions in a form (part 2 of 4). 'On_finish' checks the answers and will be evaluated by the looping function in part 3
var comp_check = {
  type: 'survey-html-form',
  preamble:
    "<p style='text-align:left'> Please answer a few questions about the task to check you know what to do.</p>",
  html: "<p style='text-align:left'>Each trial shows an event, where different causes happen different % of the time. <br>  \
            <input required type='radio' name='Q0' value='yes'>Yes <input required type='radio' name='Q0' value='no'>No<br></p> \
            <p style='text-align:left'>There is only ever one logical possible cause of each event. <br>  \
            <input required type='radio' name='correct' value='yes'>Yes <input required type='radio' name='correct' value='no'>No<br></p> \
            <p style='text-align:left'>This is an abstract situation showing one cause, but not the other, being on/present and causing the effect. <br>  \
            <img src='101.svg'></img><br> \
            <input required type='radio' name='image' value='true'>True <input required type='radio' name='image' value='false'>False<br></p> \
        <p style='text-align:left'>How many trials do you have to do? <br> \
            <input required name='t' type='number'></p>",
  on_finish: function (data) {
    if (
      (data.response.Q0 == 'yes') &
      (data.response.correct == 'no') &
      (data.response.image == 'true') &
      (data.response.t == 12)
    ) {
      // add more questions
      data.correct = true;
    } else {
      data.correct = false;
    }
    //console.log(data.correct);
  },
};

// Part 3 of 4 - feedback on the comp check. If they get it wrong, they have to go back to the instructions.
// Defines last_trial_correct as results of the previous trial, the comprehension check quiz
var loop_comp = {
  type: 'html-button-response',
  choices: function () {
    last_trial_correct = jsPsych.data.get().last(1).values()[0].correct;
    if (last_trial_correct) {
      return (choices = ['Continue to experiment']);
    } else {
      return (choices = ['Please reread the instructions and try again.']);
    }
  },
  stimulus: '',
  prompt: function () {
    last_trial_correct = jsPsych.data.get().last(1).values()[0].correct;
    if (last_trial_correct) {
      var html = `<p> Great! You understand.</p>`;
      return html;
    } else {
      var html = `<p> Oops, you got something wrong.</p>`;
      return html;
    }
  },
  on_start: function (trial) {
    trial.data = {
      last_trial_correct: jsPsych.data.get().last(1).values()[0].correct,
      //task: 'autism-loop', // redundant? try comment later
    };
    //console.log(trial.data.last_trial_correct);
  },
  on_finish: function (data) {
    if (data.last_trial_correct == false) {
      //console.log(data.last_trial_correct);
      //jsPsych.endExperiment();
    } else {
      data.last_trial_correct = true;
    }
  },
};

// Part 4/4: A looping timeline to keep the instructions on screen until they get the comp check right. Puts the previous 3 together
var loop_instruction_timeline = {
  timeline: [instructions, comp_check, loop_comp], // ie. parts 1, 2, 3
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

/******************************************************************************/
/*** Main part - probs and worlds - standard static to be used in all conditions ****/
/******************************************************************************/

/* PROBS 
(This now done inside each trial, but could be done here if we wanted to keep them static)

First: 1 5 8 5: A rare, B usually happens, but they both only work half the time
Second: 5 1 5 8: A and B both happen half the time, but A only rarely works, and B usually works
Third: 1, 7, 8, 5: all random*/

// Other global vars, modified later during each trial to contain the html contents and inserted keys which varies for each trial
var scen = '';
var cond = '';
var pic = '';
//var prob = [];
//var cb = [];

// Shuffle them, once for each participant, who will see all once each.
var all_worlds_shuffled = jsPsych.randomization.shuffle(all_worlds);

/* The main part of the experimetn is a series of functions - now in script colliderfunctions.js - bring back if it doesn't flow 
They are called here in order. */

/******************************************************************************/
/*** Situation timelines *******************************************************/
/******************************************************************************/

// Make a timeline - function 6 - puts 1-5 together

function make_timeline() {
  var timelinechunk = [];

  for (i = 0; i < all_worlds_shuffled.length; i++) {
    var all_three_shuffled = jsPsych.randomization.shuffle(all_three);
    var scenario = all_three_shuffled[0];
    var cb = jsPsych.randomization.shuffle(counterbalances)[0]; // define this in the trial generation stage
    //console.log(cb);
    var probs1 = probs[cb];
    //console.log(probs1);
    var prob = jsPsych.randomization.shuffle(probs1)[0];
    //console.log(prob);

    var intro = trial_intro(i);
    timelinechunk.push(intro);
    var piece0 = make_pic(scenario);
    timelinechunk.push(piece0);
    var piece1 = make_scenario1(scenario, all_worlds_shuffled[i], prob, cb);
    timelinechunk.push(piece1);
    var piece2 = make_scenario2(scenario, all_worlds_shuffled[i], prob);
    timelinechunk.push(piece2);
    var piece3 = make_condition(scenario, all_worlds_shuffled[i], prob);
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
    data.rowtype = 'feedback';
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
  consent1,
  consent2,
  loop_instruction_timeline,
  timelinechunk,
  //feedback_form,
  debrief,
);

jsPsych.init({
  timeline: full_timeline,
  exclusions: {
    min_width: 1500,
    min_height: 800,
  },
  show_progress_bar: true,
  on_finish: function () {
    jsPsych.endExperiment();
    window.location =
      'https://app.prolific.com/submissions/complete?cc=C4PEEOEV';
  },
});
