Program VBoxManage_Helper(output, input);

{$MODE OBJFPC}

{ Add check to see if VirtualBox is installed (or maybe installed in a different location) }

Uses
  SysUtils, StrUtils, Process;

Type
  TCommand =
  (
    FirstCommand=0,
    Modify=0,
    Restore,
    Credits,
    Exit=3,
    LastCommand=3
  );

  TField =
  (
    FirstField=0,
    DmiBIOSVendor=0,
    DmiBIOSVersion,
    DmiBIOSReleaseDate,
    DmiSystemVendor,
    DmiSystemProduct,
    DmiBoardVendor=5,
    LastField=5,
    All
  );

  TSystemVendor =
  (
    FirstVendor=0,
    Dell=0,
    HP,
    Lenovo,
    Acer,
    Asus,
    Compaq,
    Sony_VAIO,
    Samsung,
    Alienware,
    Gateway,
    Sun_Microsystems=10,
    LastVendor=10,
    Custom
  );

  TVMRecord = record
    Name:     string;
    Uuid:     string;
    firmware: string;
  end;

Const
  VBOX = 'VBoxManage';
  GEN_DEVICE_PATH = 'VBoxInternal/Devices/%s/0/Config/%s';
  LE = LineEnding;

  Field_descriptions: array[TField] of ansistring = 
  (
    'The developer of your computers firmware',
    'The version of your computers firmware',
    'The release date of your computers firmware',
    'The manufacturer of your computer',
    'The model of your computer',
    'The manufacturer of your motherboard',
    'All fields'
  );

  Bios_vendors: array[0..4] of ansistring =
  (
    'American Megatrends',
    'AMI',
    'Phoenix Technologies',
    'Insyde',
    'IBM'
  );

  System_models: array[FirstVendor..LastVendor] of array of ansistring =
  (
    ('Latitude', 'Dimension', 'Optiplex', 'Vostro', 'Inspiron'),
    ('Pavilion', 'Slimline', 'Presario', 'ProDesk', 'EliteDesk'),
    ('ThinkPad', 'IdeaPad', 'ThinkStation', 'IdeaPad', 'IdeaCentre AIO'),
    ('Aspire', 'TravelMate', 'Predator', 'Swift', 'Extensa'),
    ('ZenBook', 'VivoBook', 'G Series', 'B Series', 'Vivo AIO'),
    ('Presario', 'S series', 'DeskPro', 'SystemPro', 'Evo'),
    ('P series', 'Z series', 'S series', 'W series'),
    ('Galaxy Book', '9 series Notebook', '7 series'),
    ('15 R2', '13 R3', 'Aurora R4', 'Area 51'),
    ('N series', 'DX 4860', 'SX 2185', 'FX 6800'),
    ('Ultra 80', 'Ultra 25', 'Ultra 45', 'Ultra 24', 'Ultra 27')
  );


  {$IFDEF DARWIN}
    {$NOTE MAC OSX DETECTED }
    PLATFORM = 'MAC OSX';
    VBOX_DIR = '/usr/local/bin/';
  {$ELSE}
  {$IFDEF LINUX}
    {$NOTE LINUX DETECTED }
    PLATFOMR = 'LINUX';
    VBOX_DIR = '/usr/bin';
  {$ELSE}
  {$IFDEF WINDOWS}
    {$NOTE WINDOWS DETECTED }
    PLATFORM = 'WINDOWS';
    VBOX_DIR = '/Program Files/Oracle/VirtualBox/';
  {$ELSE}
  {$NOTE
    ### UNKNOWN OPERATING SYSTEM DETECTED ###
    This program should be able to work on any
    operating system that both FPC and VirtualBox
    support, but default directory locations are
    only supplied for OSX, Linux and Windows.}
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}

Var
  Choice:  integer;
  Command: TCommand;

{ All identifiers above are global to the whole program }



{ A very simple procedure that prevents Random() from providing 0 for dates }

Function NonZero(Num: integer): integer;

begin
  If Num = 0 then NonZero := 1;
end;



{ A procedure that tries to read from Stdin until a valid integer is given }

Function ReadIntOnly(const Lower_bound, Upper_bound: integer): integer;

var
  Input_str: string;
  Bad_char:  word;

begin
  repeat
    Write(output, LE, 'Enter an acceptable digit >');
    ReadLn(Input, Input_str);

    Val(Input_str, ReadIntOnly, Bad_char);
    If Bad_char <> 0 then
    begin
      WriteLn(output, LE + 'You can only enter digits here');
      Continue;
    end;

    If (ReadIntOnly > Upper_bound) or (ReadIntOnly < Lower_bound) then
    begin
      WriteLn(output, 'That digit is out of range for this selection');
      Continue;
    end;

    Break;
  until false;
end;



{ Enumerates through all detected VMs and returns a TVMRecord type of the selected one }

Procedure EnumerateVMs(out VM_record: TVMRecord);

var
  Sel_VM:       string;
  Firmware_str: string;
  VM_index:     integer;
  VM_count:     integer;
  Stdout:       ansistring;

begin
  RunCommandInDir(VBOX_DIR, VBOX + ' list ' + 'vms', Stdout);

  { WordCount is used to find the number of lines in this case,
    as each "word" is delimited by a new line character (LE) }

  VM_count := WordCount(Stdout, [LE]);
  WriteLn(output, 'Which VM will this command be used on? (Total: ', VM_count, ')');

  For VM_index := 1 to VM_count do WriteLn(output, VM_index, '. ', ExtractWord(VM_index, Stdout, [LE]));
  VM_index := ReadIntOnly(1, VM_count);

  Sel_VM := ExtractWord(VM_index, Stdout, [LE]);
  Sel_VM := StringReplace(Sel_VM, '"', '', [rfIgnoreCase]);


  with VM_record do
  begin
    Name := ExtractWord(1, Sel_VM, ['"']);
    Uuid := StringReplace(ExtractWord(2, Sel_VM, ['"']), ' ', '', [rfIgnoreCase]);

    RunCommandInDir(VBOX_DIR, VBOX + ' showvminfo ' + Uuid, Stdout);
    Firmware_str := ExtractWord(15, Stdout, [LE]);

    If Pos('BIOS', Firmware_str) <> 0 then
      Firmware := 'pcbios'
    Else
      Firmware := 'efi';
  end;

end;



{ A procedure that handles input for the DmiBoardVendor field }

Procedure PromptBoardVendor(SysvenStr: string; out Field_value: string);

begin
  WriteLn(output, 'NOTE:', LE,
    'On prebuilt computers, the motherboard manufacturer is almost always the same ',
    'as the system manufacturer.', LE,
    'You previously entered "', SysvenStr, '" ', 'as the system manufacturer.', LE,
    'Press enter with no input to use this value here too.', LE);

  Write('Enter value (leave blank for random value) >');
  ReadLn(input, Field_value);
end;



{ A procedure that handles input for the DmiSystemProduct field }

Procedure PromptSystemProduct(out Sysven: TSystemVendor; SysvenStr: string; out Field_value: string);

var
  Choice:      integer;
  Sysven_iter: TSystemVendor;

begin
  WriteLn(output, 'NOTE:', LE,
    'For realism, its best to choose a model/product that pairs with the selected manufactuer.', LE);

  If Sysven = Custom then  // SysvenStr will have a value if this is true
  begin
    WriteLn(output, 'You manually entered the system manufacturer "', SysvenStr, '"', LE,
      'If this manufacturer matches one of the defaults below, select it, and a valid model can be chosen for you.', LE,
      'If it doesn''t, or if you want to enter one manually still, choose "Custom"', LE);

    For Sysven_iter in TSystemVendor do WriteLn(output, Ord(Sysven_iter), '. ', Sysven_iter);

    Choice := ReadIntOnly(0, Ord(Custom));
    Sysven := TSystemVendor(Choice);

    { If the user still wanted to enter a custom model }
    If Sysven = Custom then
    begin
      WriteLn(output, 'Leaving this blank will cause a random model, from a random manufacturer to be chosen.', LE,
        'This is not recommended and may noticed as fake by a more knowledgeable person.');

      Write(output, LE, 'Enter value >');
      ReadLn(input, Field_value);
    end;
  end
  else
  begin
    WriteLn(output, 'Since you chose a random system manufacturer, a valid model/product can automatically be selected too ',
      '(this is recommended).', LE,
      'However, if you still want to enter a custom model manually, you can type it now.', LE);

    Write(output, 'Enter value (leave blank for random) >');
    ReadLn(input, Field_value);
  end;
end;



{ A procedure that asks the user for the new value of each revealing field }

Procedure ModifyVM();

var
  VM_record:    TVMRecord;

  Field:        TField;
  FieldStr:     string;
  Field_path:   string;
  Field_value:  string;

  Dummy_stdout: ansistring;

  Sysven:       TSystemVendor;
  SysvenStr:    string;

begin
  EnumerateVMs(VM_record);

  Sysven := Custom;

  For Field := FirstField to LastField do  // Try: For Field in FirstField..LastField
  begin
    Field_value := '';
    WriteLn(output, Field, ': ', Field_descriptions[Field], LE);

    Case Field of
      DmiBoardVendor:   PromptBoardVendor(SysvenStr, Field_value);
      DmiSystemProduct: PromptSystemProduct(Sysven, SysvenStr, Field_value);
    else
      { Normal value entry for most fields }
      Write(output, 'Enter value (leave blank for random value) >');
      ReadLn(input, Field_value);
    end;

    { Available default/random values }
    If Field_value = '' then
    begin
      Case Field of
        DmiBIOSVendor:      Field_value := RandomFrom(Bios_vendors);
        DmiBIOSVersion:     Field_value := Format('%d%s%d', [Random(31), '.', Random(2000)]);
        DmiBIOSReleaseDate: Field_value := Format('%d/%d/20%.2d', [NonZero(Random(13)), NonZero(Random(32)), Random(10)]);
        DmiBoardVendor:     WriteStr(Field_value, Sysven);

        DmiSystemVendor:
        begin
           Sysven := TSystemVendor(Random(Ord(LastVendor) + 1));
           WriteStr(Field_value, Sysven);
        end;

        DmiSystemProduct:
        begin
          If Sysven <> Custom then
            Field_value := RandomFrom(System_models[Sysven])
          else
            Field_value := RandomFrom(System_models[TSystemVendor(Random(Ord(LastVendor) + 1))]);
        end;

      end;
    end;

    WriteLn(output, 'Selected value for field ', Field, ': ', Field_value, LE, LE);

    { Store the system vendor so it can be re-used as the board vendor if needed }
    If Field = DmiSystemVendor then SysvenStr := Field_value;

    WriteStr(FieldStr, Field);
    Field_path := Format(GEN_DEVICE_PATH, [VM_record.Firmware, FieldStr]);

    RunCommandInDir(VBOX_DIR, Format('%s setextradata %s %s "string:%s"',
      [VBOX, VM_record.Uuid, Field_path, Field_value]), Dummy_stdout);
  end;
end;



{ A procedure that restores the default values of individual or all fields }

Procedure ResetVM();

var
  VM_record:    TVMRecord;

  Dummy_stdout: ansistring;

  Choice:       integer;
  Field:        TField;
  FieldStr:     string;
  Field_path:   string;

begin
  EnumerateVMs(VM_record);

  WriteLn(output, LE, 'Choose a field to reset the value of:');

  For Field in TField do
  begin
    WriteStr(FieldStr, Field);
    WriteLn(output, Format('%d%s %-19s %s', [Ord(Field), '.', FieldStr + ':', Field_descriptions[Field]]));
  end;

  Choice := ReadIntOnly(0, Ord(All));
  Field := TField(Choice);
  WriteStr(FieldStr, Field);

  If Field <> All then
  begin
    Field_path := Format(GEN_DEVICE_PATH, [VM_record.firmware, FieldStr]);
    RunCommandInDir(VBOX_DIR, Format('%s setextradata %s %s "<EMPTY>"', [VBOX, VM_record.Uuid, Field_path]), Dummy_stdout);
    WriteLn(output, 'Reset field "', FieldStr, '" to default value.');
  end
  else
  begin
    For Field := DmiBIOSVendor to DmiBoardVendor do  // Try: For Field in (TField - All) do, or: Fields := TField - All (or: - 1)
    begin
      WriteStr(FieldStr, Field);
      Field_path := Format(GEN_DEVICE_PATH, [VM_record.firmware, FieldStr]);
      RunCommandInDir(VBOX_DIR, Format('%s setextradata %s %s "<EMPTY>"', [VBOX, VM_record.Uuid, Field_path]), Dummy_stdout);
    end;

    WriteLn(output, 'Reset all fields to default value');
  end;

end;



{ Start of Main entry procedure }

Begin
  { Make sure random patterns don't repeat }
  Randomize();

  WriteLn(output,'[VBox Manage Helper: Cross-platform VirtualBox system information modifier]', LE,
    'PLATFORM: ', PLATFORM, LE,
    'Original Batch version by JayMontana36', LE + LE);

  WriteLn(output, 'You can quit at anytime by pressing ^C (Control + C), press enter to continue...');
  ReadLn();

  repeat
    WriteLn(output, LE, 'Currently available commands:');
    For Command in TCommand do WriteLn(output, Ord(Command), '. ', Command);
    Choice := ReadIntOnly(0, Ord(LastCommand));

    Case TCommand(Choice) of
      Modify:  ModifyVM();
      Restore: ResetVM();
      //'Credits':
      Exit:    Halt(0);
    end;
  until false;

End.
