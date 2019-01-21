unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, clTcpServer, clHttpServer, Vcl.StdCtrls, System.SyncObjs,
  clHttpHeader;

type
  TForm1 = class(TForm)
    clHttpServer1: TclHttpServer;
    Label1: TLabel;
    edtPort: TEdit;
    btnStart: TButton;
    btnStop: TButton;
    memLog: TMemo;
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure clHttpServer1Start(Sender: TObject);
    procedure clHttpServer1Stop(Sender: TObject);
    procedure clHttpServer1AcceptConnection(Sender: TObject;
      AConnection: TclUserConnection; var Handled: Boolean);
    procedure clHttpServer1CloseConnection(Sender: TObject;
      AConnection: TclUserConnection);
    procedure clHttpServer1ReceiveRequest(Sender: TObject;
      AConnection: TclHttpUserConnection; const AMethod, AUri: string;
      AHeader: TclHttpRequestHeader; ABody: TStream);
    procedure clHttpServer1SendResponse(Sender: TObject;
      AConnection: TclHttpUserConnection; AStatusCode: Integer;
      const AStatusText: string; AHeader: TclHttpResponseHeader;
      ABody: TStream);
  private
    FSynchronizer: TCriticalSection;
    FIsStop: Boolean;
    procedure PutLogMessage(const ALogMessage: string);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnStartClick(Sender: TObject);
begin
  if clHttpServer1.Active then
  begin
    ShowMessage('Server already started');
    Exit;
  end;

  FIsStop := False;

  clHttpServer1.Port := StrToInt(edtPort.Text);
  clHttpServer1.Start();
end;

procedure TForm1.btnStopClick(Sender: TObject);
begin
  FIsStop := True;
  clHttpServer1.Stop();
end;

procedure TForm1.clHttpServer1AcceptConnection(Sender: TObject;
  AConnection: TclUserConnection; var Handled: Boolean);
begin
  PutLogMessage('Accept Connection. Host: ' + AConnection.PeerIP);
end;

procedure TForm1.clHttpServer1CloseConnection(Sender: TObject;
  AConnection: TclUserConnection);
begin
  if not FIsStop then
  begin
    PutLogMessage('Close Connection. Host: ' + AConnection.PeerIP);
  end;
end;

procedure TForm1.clHttpServer1ReceiveRequest(Sender: TObject;
  AConnection: TclHttpUserConnection; const AMethod, AUri: string;
  AHeader: TclHttpRequestHeader; ABody: TStream);
begin
  PutLogMessage('Request: ' + AMethod + ' ' + AUri + ' Length: ' + IntToStr(ABody.Size));

  AConnection.ResponseHeader.ContentType := 'text/html';

  clHttpServer1.SendResponse(AConnection, 200, 'OK',
    '<html><body>You requested the ' + AUri + ' resource.</body></html>');
end;

procedure TForm1.clHttpServer1SendResponse(Sender: TObject;
  AConnection: TclHttpUserConnection; AStatusCode: Integer;
  const AStatusText: string; AHeader: TclHttpResponseHeader; ABody: TStream);
begin
  PutLogMessage('Response: ' + IntToStr(AStatusCode) + ' ' + AStatusText + ' Length: ' + IntToStr(ABody.Size));
end;

procedure TForm1.clHttpServer1Start(Sender: TObject);
begin
  PutLogMessage('=================='#13#10'Start Server');
end;

procedure TForm1.clHttpServer1Stop(Sender: TObject);
begin
  PutLogMessage('Stop Server');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FSynchronizer := TCriticalSection.Create();
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FSynchronizer.Free();
end;

procedure TForm1.PutLogMessage(const ALogMessage: string);
begin
  if not (csDestroying in ComponentState) then
  begin
    FSynchronizer.Enter();
    try
      memLog.Lines.Add(ALogMessage);
    finally
      FSynchronizer.Leave();
    end;
  end;
end;

end.
