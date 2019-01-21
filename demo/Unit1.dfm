object Form1: TForm1
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'HTTP Server demo'
  ClientHeight = 299
  ClientWidth = 458
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 11
    Width = 20
    Height = 13
    Caption = 'Port'
  end
  object edtPort: TEdit
    Left = 58
    Top = 8
    Width = 79
    Height = 21
    TabOrder = 0
    Text = '80'
  end
  object btnStart: TButton
    Left = 152
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 1
    OnClick = btnStartClick
  end
  object btnStop: TButton
    Left = 248
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Stop'
    TabOrder = 2
    OnClick = btnStopClick
  end
  object memLog: TMemo
    Left = 8
    Top = 55
    Width = 441
    Height = 236
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 3
  end
  object clHttpServer1: TclHttpServer
    ServerName = 'Clever Internet Suite HTTP service'
    Port = 0
    OnStart = clHttpServer1Start
    OnStop = clHttpServer1Stop
    OnAcceptConnection = clHttpServer1AcceptConnection
    OnCloseConnection = clHttpServer1CloseConnection
    OnReceiveRequest = clHttpServer1ReceiveRequest
    OnSendResponse = clHttpServer1SendResponse
    Left = 376
    Top = 8
  end
end
