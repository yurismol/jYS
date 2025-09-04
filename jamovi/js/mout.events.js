const events = {
  onChange_iqr: function(ui) {
    //console.log(ui.outlcheck.value())
    if (ui.outlcheck.value()!=="IQR") ui.boxpl.setValue(false)
    if (ui.outlcheck.value() =="ZS")  ui.norm.setValue(true)
  }
};

module.exports = events;
