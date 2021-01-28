loadiDialog : dialog {
label = "Spec tot";

: boxed_radio_column {    
  label = "Specification type";
  : radio_button {        
    label = "&Bars";
    key = "sp_bars";
    value = "1";
   }
: radio_button {       
  label = "&Nets";
  key = "sp_nets";
 }
}

: boxed_column {    
  label = "Net options";
  key="n_opt";

  : edit_box {     
  label = "&Increase slab nets";
  key = "sp_is";
  edit_width = 6;
 
}
: edit_box {      
  label = "&Increase wall nets";
  key = "sp_iw";
  edit_width = 6;
}

  : toggle {
  label = "Check dims";
  key = "sp_cdim";
  value = "0";
}

  : toggle {
  label = "Round to 0.5";
  key = "sp_r";
  value = "0";
}

}

: boxed_column {    
  label = "Bar options";
: text_part {
 label = "Arche sub spec";
}

  : button {    
    label = "S arche";
    key = "sarche";
    width = 8;
    fixed_width = true;
  }
    : button {    
    label = "S arche tot";
    key = "sarchetot";
    width = 8;
    fixed_width = true;
  }


}

: row {         
  : spacer { width = 1; }
  : button {    
    label = "OK";
    is_default = true;
    key = "accept";
    width = 8;
    fixed_width = true;
  }
  : button {   
    label = "Cancel";
    is_cancel = true;
    key = "cancel";
    width = 8;
    fixed_width = true;
  }
  : spacer { width = 1;}
}

}