function showErrorToaster(title, text) {
      $.uiAlert({
textHead: title,
text: text,
bgcolor: '#DB2828', // background-color
textcolor: '#fff', // color
position: 'top-center',// position . top And bottom ||  left / center / right
icon: 'remove circle', // icon in semantic-UI
time: 4, // time
  })
}

function showMessageToaster(title, text) {
      $.uiAlert({
textHead: title,
text: text,
bgcolor: '#19c3aa', // background-color
textcolor: '#fff', // color
position: 'top-center',// position . top And bottom ||  left / center / right
icon: 'check circle outline', // icon in semantic-UI
time: 3, // time
  })
}

function showServerErrorToaster(text) {
      $.uiAlert({
textHead: text,
text: 'Server Error - please check whether application server is running',
bgcolor: '#DB2828', // background-color
textcolor: '#fff', // color
position: 'top-center',// position . top And bottom ||  left / center / right
icon: 'remove circle', // icon in semantic-UI
time: 5, // time
  })
}

function getUrlParameter(name) {
    name = name.replace(/[\[]/, '\\[').replace(/[\]]/, '\\]');
    var regex = new RegExp('[\\?&]' + name + '=([^&#]*)');
    var results = regex.exec(location.search);
    return results === null ? '' : decodeURIComponent(results[1].replace(/\+/g, ' '));
};

function performAction(id, url) {
$(this)
  .api({
    action: url,
    on: 'now',
    method : 'POST',
    beforeSend: function(settings) {
      settings.urlData = {
        id: id
      };
      // cancel request
      if(!isLoggedIn) {
        //showErrorToaster('Authentication Error', 'Please login');
        //return false;
      }
      return settings;
    },
    beforeXHR: function(xhr) {
      // adjust XHR with additional headers
      xhr.setRequestHeader('x-auth-toke', id);
      return xhr;
    },
    successTest: function(response) {
        console.log('response: ' + response);
      return response.status == true || false;
    },
    onSuccess: function(response) {
      // valid response and response.success = true
      showMessageToaster('Success', response.message);
      $("#link_" + id).attr("href", response.link);
      $("#link_" + id).show();
      //$("#box_" + id).hide();
    },
    onFailure: function(response) {
        showErrorToaster('Application Error', response.message);
      // request failed, or valid response but response.success = false
    },
    onError: function(errorMessage) {
      showErrorToaster('Application Error', errorMessage);
      // invalid response
    },
    onAbort: function(errorMessage) {
      showErrorToaster('User Error', errorMessage);
      // navigated to a new page, CORS issue, or user canceled request
    }
  });
}
function performUpdate(id, url) {
var value = $("#select_" + id).find("input[name='status']").val();
console.log("value in perform update: " + value);
$(this)
  .api({
    action: url,
    on: 'now',
    method : 'POST',
    beforeSend: function(settings) {
      settings.urlData = {
        id: id,
        status: value
      };
      // cancel request
      if(!isLoggedIn) {
        //showErrorToaster('Authentication Error', 'Please login');
        //return false;
      }
      return settings;
    },
    beforeXHR: function(xhr) {
      // adjust XHR with additional headers
      xhr.setRequestHeader('x-auth-toke', id);
      return xhr;
    },
    successTest: function(response) {
        console.log('response: ' + response);
      return response.status == true || false;
    },
    onSuccess: function(response) {
      // valid response and response.success = true
      showMessageToaster('Success', response.message);
      //$("#box_" + id).hide();
    },
    onFailure: function(response) {
        showErrorToaster('Application Error', response.message);
      // request failed, or valid response but response.success = false
    },
    onError: function(errorMessage) {
      showErrorToaster('Application Error', errorMessage);
      // invalid response
    },
    onAbort: function(errorMessage) {
      showErrorToaster('User Error', errorMessage);
      // navigated to a new page, CORS issue, or user canceled request
    }
  });
}
function saveMatchingRules() {
var url = 'save matching rule';
$('.form .submit')
  .api({
    action: url,
    on: 'now',
    serializeForm: true,
    method : 'POST',
    //data: { session: 22 },
    beforeSend: function(settings) {
      // cancel request
      if(!isLoggedIn) {
        //showErrorToaster('Authentication Error', 'Please login');
        //return false;
      }
      return settings;
    },
    beforeXHR: function(xhr) {
      // adjust XHR with additional headers
      xhr.setRequestHeader('x-auth-toke', id);
      return xhr;
    },
    successTest: function(response) {
        console.log('response: ' + response);
      return response.status == true || false;
    },
    onSuccess: function(response) {
      // valid response and response.success = true
      showMessageToaster('Success', response.message);
      //$("#box_" + id).hide();
    },
    onFailure: function(response) {
        showErrorToaster('Application Error', response.message);
      // request failed, or valid response but response.success = false
    },
    onError: function(errorMessage) {
      showErrorToaster('Application Error', errorMessage);
      // invalid response
    },
    onAbort: function(errorMessage) {
      showErrorToaster('User Error', errorMessage);
      // navigated to a new page, CORS issue, or user canceled request
    }
  });
}

function updateJob(id, bookmark) {
var url = 'save bookmark';
$(this)
  .api({
    action: url,
    on: 'now',
    //serializeForm: true,
    method : 'POST',
    contentType: "application/json",
    dataType: "json",
    data: { id: id, bookmark: bookmark },
    beforeSend: function(settings) {
      settings.data = JSON.stringify(settings.data);
      console.log(settings.data);
      return settings;
    },
    beforeXHR: function(xhr) {
      // adjust XHR with additional headers
      //xhr.setRequestHeader('x-auth-toke', id);
      return xhr;
    },
    successTest: function(response) {
        console.log('response: ' + response);
      return response.status == true || false;
    },
    onSuccess: function(response) {
      // valid response and response.success = true
      showMessageToaster('Success', response.message);
      $("#bookmark_" + id).addClass("red");
    },
    onFailure: function(response) {
        showErrorToaster('Application Error', response.message);
      // request failed, or valid response but response.success = false
    },
    onError: function(errorMessage) {
      showErrorToaster('Application Error', errorMessage);
      // invalid response
    },
    onAbort: function(errorMessage) {
      showErrorToaster('User Error', errorMessage);
      // navigated to a new page, CORS issue, or user canceled request
    }
  });
}

function downloadJobPdf(id) {
var url = 'download job pdf';
$(this)
  .api({
    action: url,
    on: 'now',
    //serializeForm: true,
    method : 'POST',
    contentType: "application/json",
    dataType: "json",
    data: { id: id},
    beforeSend: function(settings) {
      settings.data = JSON.stringify(settings.data);
      console.log(settings.data);
      return settings;
    },
    beforeXHR: function(xhr) {
      // adjust XHR with additional headers
      //xhr.setRequestHeader('x-auth-toke', id);
      return xhr;
    },
    successTest: function(response) {
        console.log('response: ' + response);
      return response.status == true || false;
    },
    onSuccess: function(response) {
      // valid response and response.success = true
      showMessageToaster('Success', response.message);
      $("#bookmark_" + id).addClass("red");
    },
    onFailure: function(response) {
        showErrorToaster('Application Error', response.message);
      // request failed, or valid response but response.success = false
    },
    onError: function(errorMessage) {
      showErrorToaster('Application Error', errorMessage);
      // invalid response
    },
    onAbort: function(errorMessage) {
      showErrorToaster('User Error', errorMessage);
      // navigated to a new page, CORS issue, or user canceled request
    }
  });
}

