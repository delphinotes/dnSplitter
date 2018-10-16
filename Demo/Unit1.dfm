object Form1: TForm1
  Left = 436
  Top = 230
  Caption = 'Form1'
  ClientHeight = 307
  ClientWidth = 476
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  ShowHint = True
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 69
    Width = 476
    Height = 159
    Align = alClient
    BevelOuter = bvLowered
    Caption = 'Panel1'
    TabOrder = 0
    object Panel4: TPanel
      Left = 1
      Top = 1
      Width = 188
      Height = 157
      Align = alLeft
      BevelOuter = bvNone
      Caption = 'Panel4'
      TabOrder = 0
    end
    object dnSplitter1: TdnSplitter
      Left = 189
      Top = 1
      Action = Action1
      Beveled = True
      MinSize = 120
      AlignControl = Panel4
      ButtonAlign = baLeftTop
      ButtonPosition = 4
      ButtonWidth = 40
    end
    object Panel3: TPanel
      Left = 197
      Top = 1
      Width = 278
      Height = 157
      Align = alClient
      BevelOuter = bvNone
      Caption = 'Panel3'
      TabOrder = 2
    end
  end
  object dnSplitter2: TdnSplitter
    Left = 0
    Top = 228
    Action = Action2
    Beveled = True
    AlignControl = Panel2
  end
  object Panel2: TPanel
    Left = 0
    Top = 236
    Width = 476
    Height = 71
    Align = alBottom
    Caption = 'Panel2'
    TabOrder = 2
  end
  object Panel5: TPanel
    Left = 0
    Top = 0
    Width = 476
    Height = 69
    Align = alTop
    BevelOuter = bvLowered
    TabOrder = 3
    object Button1: TButton
      Left = 8
      Top = 8
      Width = 181
      Height = 25
      Action = Action1
      TabOrder = 0
    end
    object Button2: TButton
      Left = 8
      Top = 36
      Width = 181
      Height = 25
      Action = Action2
      TabOrder = 1
    end
    object CheckBox1: TCheckBox
      Left = 200
      Top = 8
      Width = 125
      Height = 17
      Action = Action1
      State = cbChecked
      TabOrder = 2
    end
    object CheckBox2: TCheckBox
      Left = 200
      Top = 36
      Width = 125
      Height = 17
      Action = Action2
      State = cbChecked
      TabOrder = 3
    end
  end
  object ActionList1: TActionList
    Left = 336
    Top = 16
    object Action1: TAction
      AutoCheck = True
      Caption = 'Hide/Show Splitter 1'
      Checked = True
      Hint = 'Hide/Show Splitter1'
    end
    object Action2: TAction
      AutoCheck = True
      Caption = 'Hide/Show Splitter2'
      Checked = True
      Hint = 'Hide/Show Splitter2'
    end
  end
end
