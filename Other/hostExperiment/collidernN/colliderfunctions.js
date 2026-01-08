/******************************************************************************/
/*** MAIN EXPERIMENT FUNCTIONS - GENERATE DIFFERENT TRIALS FOR EACH PPT  ******/
/******************************************************************************/

/* The main body of collider experiment - out separate to be tidy - go to colliderexp.js for 
front and back matter and admin, and the actual trial generation using these functions */

/******************************************************************************/
/*** The five functions in order: pic, scenx2, cond, ans  ************************/
/******************************************************************************/

function trial_intro(trial_number) {
  var intro = {
    type: 'html-button-response',
    stimulus:
      "<p>The next trial is # <b><u style = 'color:#B22C0F'>" +
      JSON.stringify(trial_number + 1) +
      '</u></b> of 12. </p>',
    choices: ['Start trial'],
  };
  return intro;
}

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
    stimulus_height: 500,
    maintain_aspect_ratio: true,
    prompt: '<b>The situation is ...</b>' + scenario.description + '',
    choices: ['Next'],
  };
  return trial;
}

// FUNCTION 2: SCEN1of2: Make scenario for each trial. (ie screen 2 of 5) -- setting the specific situation part 1
function make_scenario1(scenario, world, prob, cb) {
  if (scenario == job) {
    scen =
      '<p style="text-align:left">The product has Feature <b>A</b> and Feature <b>B</b>, \
     and the client will always <br> accept the product if they see a working demonstration of <b><u style = "color:#B22C0F"> ' +
      scenario.scenOpt1[world.S] +
      "</u></b>.</p>\
     <p style='text-align:left'>The company presents Feature <b>A</b> <b><u style = 'color:#B22C0F'>" +
      prob[0] +
      "</u></b> of the time and Feature <b>B</b> <b><u style = 'color:#B22C0F'> " +
      prob[2] +
      " </u></b> of the time.<br>\
      <p style='text-align:left'>The underlying laptop files for Feature <b>A</b> are sometimes corrupted: they only work <b><u style = 'color:#B22C0F'>" +
      prob[1] +
      " </u></b> of the time, <br> whether the feature is presented or not. <br>\
       The underlying laptop files for Feature <b>B</b> are sometimes corrupted: they only work <b><u style = 'color:#B22C0F'> " +
      prob[3] +
      ' </u></b> of the time, <br> whether the feature is presented or not.</p>';
  } else if (scenario == cook) {
    scen =
      '<p style="text-align:left"> <b><u style = "color:#B22C0F">' +
      prob[0] +
      " </u></b> of the cafes have main dishes and <b><u style = 'color:#B22C0F'>" +
      prob[2] +
      " </u></b> have desserts.</p>\
      <p style='text-align:left'>The person wants to eat a main dish <b><u style = 'color:#B22C0F'>" +
      prob[1] +
      " </u></b> of the time and wants to eat a dessert <b><u style = 'color:#B22C0F'>" +
      prob[3] +
      " </u></b> of the time.<br>\
    <p style='text-align:left'>The person enters the cafe to order if <b><u style = 'color:#B22C0F'>" +
      scenario.scenOpt1[world.S] +
      '</u></b> on the menu and the person wants to eat what is on the menu.</p>';
  } else if (scenario == group) {
    scen =
      '<p style="text-align:left">The <b>first student</b> attends <b><u style = "color:#B22C0F">' +
      prob[0] +
      " </u></b> of the time and the <b>second student</b> attends <b><u style = 'color:#B22C0F'>" +
      prob[2] +
      " </u></b> of the time.</p>\
     <p style='text-align:left'>The first student has a good morning <b><u style = 'color:#B22C0F'>" +
      prob[1] +
      " </u></b> of the time, and the second student has a good morning <b><u style = 'color:#B22C0F'>" +
      prob[3] +
      " </u></b> of the time. <br>\
     <p style='text-align:left'>A good discussion always happens when <b><u style = 'color:#B22C0F'> " +
      scenario.scenOpt1[world.S] +
      ' </u></b> and had a good morning.</p>';
  }
  var trial = {
    type: 'html-button-response-custom',
    //cb: cb,
    stimulus: scen,
    //prompt: '<b>Schematic of situation</b>',
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
      data.rowtype = 'stim';
      data.trialtype = world.trialtype;
      saveDataLine(data);
    },
  };
  return trial;
}

// FUNCTION 3: SCEN2of2: Make scenario for each trial. (ie screen 3 of 5) -- setting the specific situation part 2
function make_scenario2(scenario, world, prob) {
  if (scenario == job) {
    scen2 =
      '<p style="display:inline-block;color:blue;">A presented ' +
      prob[0] +
      ' of time. File works ' +
      prob[1] +
      ' of time. <img src=' +
      scenario.fig[world.S] +
      '> B presented ' +
      prob[2] +
      ' of time. File works ' +
      prob[3] +
      ' of time.</p>';
  } else if (scenario == cook) {
    scen2 =
      '<p style="display:inline-block;color:blue;"> Main dish on ' +
      prob[0] +
      ' of menus. Person wants to eat main dish ' +
      prob[1] +
      ' of time. <img src=' +
      scenario.fig[world.S] +
      '> Dessert on ' +
      prob[2] +
      ' of menus. Person wants to eat dessert ' +
      prob[3] +
      ' of time. </p>';
  } else if (scenario == group) {
    scen2 =
      '<p style="display:inline-block;color:blue;"> First student attends ' +
      prob[0] +
      ' of time. Has good morning ' +
      prob[1] +
      ' of time. <img src=' +
      scenario.fig[world.S] +
      '> Second student attends ' +
      prob[2] +
      ' of time. Has good morning ' +
      prob[3] +
      ' of time. </p>';
  }
  var trial = {
    type: 'html-button-response-custom',
    stimulus: scen2,
    prompt: '<b>Schematic of situation</b>',
    choices: ['Next'],
    /* on_finish: function (data) {
      //data.cb = cb;
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
    }, */
  };
  return trial;
}

// FUNCTION 4: COND: Make condition for each trial. (ie screen 4 of 5) -- what happened THIS TIME
function make_condition(condition, world, prob) {
  if (condition == job) {
    cond =
      '<p style="display:inline-block;color:blue;"> Feature A presented ' +
      prob[0] +
      ' of time. File works ' +
      prob[1] +
      ' of time.<img src=' +
      world.job +
      '> Feature B presented ' +
      prob[2] +
      ' of time. File works ' +
      prob[3] +
      " of time.</p> </b> <p>On this occasion, the presenter <b><u style = 'color:#B22C0F'>" +
      condition.Atext[world.A] +
      " </u></b> and <b><u style = 'color:#B22C0F'>" +
      condition.Btext[world.B] +
      " </u></b> and the client <b><u style = 'color:#B22C0F'>" +
      condition.Etext[world.E] +
      '</u></b> the work package.</p>';
  } else if (condition == cook) {
    cond =
      '<p style="display:inline-block;color:blue;"> Main dish on ' +
      prob[0] +
      ' of menus. Person wants to eat main dish ' +
      prob[1] +
      ' of time. <img src=' +
      world.cook +
      '> Dessert on ' +
      prob[2] +
      ' of menus. Person wants to eat dessert ' +
      prob[3] +
      " of time. </p> </b> <p>On this occasion, the <b><u style = 'color:#B22C0F'>" +
      condition.Atext[world.A] +
      " </u></b> and the <b><u style = 'color:#B22C0F'>" +
      condition.Btext[world.B] +
      " </u></b> and the person <b><u style = 'color:#B22C0F'>" +
      condition.Etext[world.E] +
      '.</u></b></p>';
  } else if (condition == group) {
    cond =
      '<p style="display:inline-block;color:blue;"> First student attends ' +
      prob[0] +
      ' of time. Has good morning ' +
      prob[1] +
      ' of time. <img src=' +
      world.group +
      '> Second student attends ' +
      prob[2] +
      ' of time. Has good morning ' +
      prob[3] +
      " of time. </p> <p>On this occasion, the first student <b><u style = 'color:#B22C0F'>" +
      condition.Atext[world.A] +
      " </u></b> and the second student <b><u style = 'color:#B22C0F'>" +
      condition.Btext[world.B] +
      " </u></b> and there was <b><u style = 'color:#B22C0F'>" +
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
  'The presenter presented Feature A',
  'The presenter did not present Feature A',
  'The files underlying Feature A were fine',
  'The files underlying Feature A were corrupted',
  'The presented presented Feature B',
  'The presented did not present Feature B',
  'The files underlying Feature B were fine',
  'The files underlying Feature B were corrupted',
];

var cook_ans = [
  'The main dish was on the menu',
  'The main dish was not on the menu',
  'The person wanted to eat the main dish',
  'The person did not want to eat the main dish',
  'The dessert was on the menu',
  'The dessert was not on the menu',
  'The person wanted to eat the dessert',
  'The person did not want to eat the dessert',
];

var group_ans = [
  'The first student attended',
  'The first student did not attend',
  'The first student had had a good morning',
  'The first student had not had a good morning',
  'The second student attended',
  'The second student did not attend',
  'The second student had had a good morning',
  'The second student had not had a good morning',
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
  var possAns = world.maxAns; // the indices of possible answers for this world were set in the world object CHANGED TO MAXANS FOR PILOT
  anstext = [];
  for (var i = 0; i < possAns.length; i++) {
    anstext[i] = ans[possAns[i]]; // take the text for those permissable indices
  }
  actual_ans = []; // Make an array for permissble answers
  for (var j = 0; j < anstext.length; j++) {
    actual_ans.push(
      '<input type="radio" name="q1" required value=' +
        JSON.stringify(anstext[j]) + // where the text itself is stored as the value to save for later as our data
        ' >' +
        anstext[j] + // and the text is also displayed against the buttons
        '<br>',
    );
  }
  actual_ans.push(
    '<p> <br> <br> Reminder of general rates and what happened: <br> <br> <br>' +
      cond +
      '</p>',
  );

  var trial = {
    type: 'survey-html-form',
    preamble: '<b>What is the best explanation for what happened?</b> <br>',
    html: actual_ans.join(''), // The .join('') is important to removes commas that are inserted by default when parts of an array are joined.
    post_trial_gap: 500,
    on_finish: function (data) {
      data.answer = data.response.q1;
      data.rowtype = 'ans';
      data.trialtype = world.trialtype;
      saveDataLine(data);
    },
  };
  return trial;
}
