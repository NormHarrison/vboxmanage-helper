Program VBoxManage_Helper(output, input);

{$MODE OBJFPC}

Uses
  SysUtils, StrUtils, Process;

Type
  TCommand =
  (
    Modify,
    Restore,
    Credits
  );

  TField: =
  (
    DmiBIOSVendor,
    DmiBIOSVersion,
    DmiBIOSReleaseDate,
    DmiSystemVendor,
    DmiSystemProduct,
    DmiBoardVendor
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

  Exposing_fields: array of ansistring =
  (
    'DmiBIOSVendor',
    'DmiBIOSVersion',
    'DmiBIOSReleaseDate',
    'DmiSystemVendor',
    'DmiSystemProduct',
    'DmiBoardVendor'
  );

  Field_descriptions: array of ansistring = 
  (
    'The developer of your computers firmware',
    'The version of your computers firmware',
    'The release date of your computers firmware',
    'The manufacturer of your computer',
    'The model of your computer',
    'The manufacturer of your motherboard'
  );

  System_vendors: array of ansistring =
  (
    'Dell',
    'Hewlet-Packard',
    'Lenovo',
    'Acer',
    'Asus',
    'Compaq',
    'Sony VAIO',
    'Samsung',
    'Alienware',
    'Gateway',
    'Sun Microsystems'
  );

  System_models: array of array of ansistring =
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
  Bios_vendors: array of ansistring =
  (
    'American Megatrends',
    'AMI',
    'Phoenix Technologies',
    'Insyde',
    'IBM'
  );

  Choice:  integer;
  Command: TCommand;

{ All data above is global to the whole program }



{ A procedure that tries to read from Stdin until a valid integer is given }

Function ReadIntOnly(const Lower_bound, Upper_bound: integer; const Allow_exit: boolean): integer;

var
  Input_str: string;
  Bad_char:  word;

begin
  repeat
    Write(output, LE, 'Enter an acceptable digit');

    If Allow_exit then
      Write(output, ', use "skip" to skip this selection ');

    Write(output, '>');
    ReadLn(Input, Input_str);

    If Allow_exit and (Input_str = 'skip') then Exit(-1);

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

    Exit();

  until false;
end;



{ Enumerates through all detected VMs and returns a VMRecord of the selected one }

Procedure EnumerateVMs(out VM_record: TVMRecord);

var
  Sel_VM:       string;
  Firmware_str: string;
  VM_index:     integer;
  VM_count:     integer;
  Stdout:       ansistring;

begin
  RunCommandInDir(VBOX_DIR, VBOX + ' list ' + 'vms', Stdout);

  VM_count := WordCount(Stdout, [LE]);
  WriteLn(output, 'Which VM will this command be used on? (Total: ', VM_count, ')');

  For VM_index := 1 to VM_count do WriteLn(output, VM_index, '. ', ExtractWord(VM_index, Stdout, [LE]));
  VM_index := ReadIntOnly(1, VM_count, false);

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



{ A procedure that asks the user for the new value of each field in the Exposing_fields array }

Procedure ModifyVM();

var
  VM_record:    TVMRecord;
  Arr_iter:     integer;
  Sysven:       string;
  Sysven_index: integer;

  Field_path:   string;
  Field_value:  string;
  Field_index:  integer;
  Dummy_stdout: ansistring;

begin
  EnumerateVMs(VM_record);

  Sysven_index := -1;

  For Field_index := 0 to High(Exposing_fields) do
  begin
    Field_value := '';
    WriteLn(output, Format('%s %s', [Exposing_fields[Field_index] + ':', Field_descriptions[Field_index]]), LE);

    If Exposing_fields[Field_index] = 'DmiBoardVendor' then
    begin
      WriteLn(output, 'NOTE:', LE,
      'On prebuilt computers, the motherboard manufacturer is almost always the same ',
      'as the system manufacturer.', LE,
      'You previously entered "', Sysven, '" ', 'as the system manufacturer. ',
      'Press enter with no input to use this value here too.', LE);
    end;

    If Exposing_fields[Field_index] = 'DmiSystemProduct' then
    begin

      WriteLn(output, 'NOTE:', LE,
      'For realism, its best to choose a model/product that pairs with the selected manufactuer.', LE);

      If Sysven_index = -1 then
      begin
        WriteLn(output, 'You manually entered the system manufacturer "', Sysven, '"', LE,
        'If this manufacturer matches one of the defaults below, select it, and a valid model can be chosen for you.', LE,
        'If it doesn''t, or if you want to enter one manually, type "skip"', LE);

        For Arr_iter := 0 to High(System_vendors) do WriteLn(output, Arr_iter, '. ', System_vendors[Arr_iter]);
        Sysven_index := ReadIntOnly(0, High(System_vendors), true);

        { If the user still wanted to enter a custom model }
        If Sysven_index = -1 then
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
        Write(output, 'Enter value, or leave blank for random >');
        ReadLn(input, Field_value);
      end;
    end

    else

    begin
      { Normal value entry for most fields }
      Write(output, 'Enter value (leave blank for random value) >');
      ReadLn(input, Field_value);
    end;

    { Available default/random values }
    If Field_value = '' then
    begin
      Case Exposing_fields[Field_index] of
        'DmiBIOSVendor':      Field_value := RandomFrom(Bios_vendors);
        'DmiBIOSVersion':     Field_value := Format('%d%s%d', [Random(31), '.', Random(2000)]);
        'DmiBIOSReleaseDate': Field_value := Format('%d/%d/20%.2d', [Random(13), Random(32), Random(10)]);
        'DmiBoardVendor':     Field_value := Sysven;

        'DmiSystemVendor':
         begin
           Sysven_index := Random(Length(System_vendors));
           Field_value := System_vendors[Sysven_index];
         end;

        'DmiSystemProduct':
        begin
          If Sysven_index <> -1 then
            Field_value := RandomFrom(System_models[Sysven_index])
          else
            Field_value := RandomFrom(System_models[Random(Length(System_models))]);
        end;
      end;
    end;

    WriteLn(output, 'Selected value for field ', Exposing_fields[Field_index], ': ', Field_value, LE, LE);

    If Exposing_fields[Field_index] = 'DmiSystemVendor' then Sysven := Field_value;

    Field_path := Format(GEN_DEVICE_PATH, [VM_record.Firmware, Exposing_fields[Field_index]]);

    RunCommandInDir(VBOX_DIR, Format('%s setextradata %s %s "string:%s"',
    [VBOX, VM_record.Uuid, Field_path, Field_value]), Dummy_stdout);
  end;
end;



{ A procedure that restores the default values of individual or all fields }

Procedure ResetVM();

var
  Field_index: integer;

begin
  For Field_index := 0 to High(Exposing_fields) do
  begin
    WriteLn(output, Format('%d%s %-19s %s', [Field_index, '.', Exposing_fields[Field_index] + ':', Field_descriptions[Field_index]]));
  end;

end;



{ Start of Main entry procedure }

Begin
  { Make sure random patterns don't repeat }
  Randomize();

  WriteLn(output,'[VBox Manage Helper: Cross-platform VirtualBox system information modifier] ',
  'PLATFORM: ' + PLATFORM, LE, 'Original version by JayMontana36', LE + LE);

  WriteLn(output, 'You can quit at anytime by pressing ^C (Control + C), press enter to continue...');
  ReadLn();

  repeat
    WriteLn(output, 'Currently available commands:');
    For Command in TCommand do WriteLn(output, Ord(Command), '. ', Command);

    // Use variant return type/out parameter so a conversion from int to the custom type isn't needed?
    Choice := ReadIntOnly(0, Ord(Credits), false);

    Case TCommand(Choice) of
      Modify:  ModifyVM();
      Restore: ResetVM();
      //'Credits':
    end;

  until false;
End.
