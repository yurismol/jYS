const events = {
  onChange_grp: function(ui) {
    //console.log(ui.outlcheck.value())
    if (ui.groups.value()===null) ui.sepROC.setValue(false)
  }
};

module.exports = events;
