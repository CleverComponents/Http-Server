unit Unit1;

interface

{$WARN SYMBOL_PLATFORM OFF}

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, clTcpServer, clHttpServer, Vcl.StdCtrls, System.SyncObjs,
  clHttpHeader, clTcpServerTls, clCertificateStore, clCertificate, clUtils, clCryptAPI, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    edtHttpPort: TEdit;
    btnStart: TButton;
    btnStop: TButton;
    memLog: TMemo;
    CertificateStore: TclCertificateStore;
    HttpServer: TclHttpServer;
    HttpsServer: TclHttpServer;
    pnlLogo: TPanel;
    imLogoLeft: TImage;
    imLogoMiggle: TImage;
    imLogoRight: TImage;
    cbHttpBinding: TCheckBox;
    cbHttpsBinding: TCheckBox;
    edtHttpsPort: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    edtWebsiteRootDir: TEdit;
    btnBrowse: TButton;
    FileOpenDialog: TFileOpenDialog;
    Label3: TLabel;
    edtCertificate: TEdit;
    btnCertificate: TButton;
    OpenDialog: TOpenDialog;
    Label4: TLabel;
    edtPassword: TEdit;
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure HttpsServerStart(Sender: TObject);
    procedure HttpsServerStop(Sender: TObject);
    procedure HttpsServerAcceptConnection(Sender: TObject;
      AConnection: TclUserConnection; var Handled: Boolean);
    procedure HttpsServerCloseConnection(Sender: TObject;
      AConnection: TclUserConnection);
    procedure HttpsServerReceiveRequest(Sender: TObject;
      AConnection: TclHttpUserConnection; const AMethod, AUri: string;
      AHeader: TclHttpRequestHeader; ABody: TStream);
    procedure HttpsServerSendResponse(Sender: TObject;
      AConnection: TclHttpUserConnection; AStatusCode: Integer;
      const AStatusText: string; AHeader: TclHttpResponseHeader;
      ABody: TStream);
    procedure HttpsServerGetCertificate(Sender: TObject;
      var ACertificate: TclCertificate; AExtraCerts: TclCertificateList;
      var Handled: Boolean);
    procedure HttpServerStart(Sender: TObject);
    procedure HttpServerStop(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure btnCertificateClick(Sender: TObject);
  private
    FSynchronizer: TCriticalSection;
    FIsStop: Boolean;
    procedure PutLogMessage(const ALogMessage: string);
    procedure UpdateStatus;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

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

procedure TForm1.UpdateStatus;
const
  states: array[Boolean] of string = ('HTTP Server demo', 'HTTP Server demo - Started');
begin
  Caption := states[HttpServer.Active or HttpsServer.Active];
end;

procedure TForm1.btnBrowseClick(Sender: TObject);
begin
  if FileOpenDialog.Execute() then
  begin
    edtWebsiteRootDir.Text := FileOpenDialog.FileName;
  end;
end;

procedure TForm1.btnCertificateClick(Sender: TObject);
begin
  if OpenDialog.Execute() then
  begin
    edtCertificate.Text := OpenDialog.FileName;
  end;
end;

procedure TForm1.btnStartClick(Sender: TObject);
var
  cert: TclCertificate;
begin
  if HttpServer.Active or HttpsServer.Active then
  begin
    ShowMessage('Server already started');
    Exit;
  end;

  FIsStop := False;

  if FileExists(edtCertificate.Text) then
  begin
    CertificateStore.ImportFromPFX(edtCertificate.Text, edtPassword.Text);
  end else
  begin
    CertificateStore.ValidFrom := Now();
    CertificateStore.ValidTo := Now() + 365;
    CertificateStore.EnhancedKeyUsage.Add(szOID_PKIX_KP_SERVER_AUTH);
    cert := CertificateStore.CreateSelfSigned('CN=localhost,O=CleverComponents,E=CleverTester@company.mail', 0);
    CertificateStore.Items.Add(cert);
  end;

  if cbHttpBinding.Checked then
  begin
    HttpServer.Port := StrToInt(edtHttpPort.Text);
    HttpServer.Start();
  end;

  if cbHttpsBinding.Checked then
  begin
    HttpsServer.Port := StrToInt(edtHttpsPort.Text);
    HttpsServer.Start();
  end;

  UpdateStatus();
end;

procedure TForm1.btnStopClick(Sender: TObject);
begin
  FIsStop := True;
  HttpServer.Stop();
  HttpsServer.Stop();
  CertificateStore.Close();
  UpdateStatus();
end;

procedure TForm1.HttpServerStart(Sender: TObject);
begin
  PutLogMessage('=================='#13#10'Start HTTP Server');
end;

procedure TForm1.HttpServerStop(Sender: TObject);
begin
  PutLogMessage('Stop HTTP Server');
end;

procedure TForm1.HttpsServerStart(Sender: TObject);
begin
  PutLogMessage('=================='#13#10'Start HTTPS Server');
end;

procedure TForm1.HttpsServerStop(Sender: TObject);
begin
  PutLogMessage('Stop HTTPS Server');
end;

procedure TForm1.HttpsServerAcceptConnection(Sender: TObject;
  AConnection: TclUserConnection; var Handled: Boolean);
begin
  PutLogMessage('Accept Connection. Host: ' + AConnection.PeerIP);
end;

procedure TForm1.HttpsServerCloseConnection(Sender: TObject;
  AConnection: TclUserConnection);
begin
  if not FIsStop then
  begin
    PutLogMessage('Close Connection. Host: ' + AConnection.PeerIP);
  end;
end;

procedure TForm1.HttpsServerGetCertificate(Sender: TObject;
  var ACertificate: TclCertificate; AExtraCerts: TclCertificateList;
  var Handled: Boolean);
begin
  ACertificate := CertificateStore.Items[0];
  Handled := True;
end;

procedure TForm1.HttpsServerSendResponse(Sender: TObject;
  AConnection: TclHttpUserConnection; AStatusCode: Integer;
  const AStatusText: string; AHeader: TclHttpResponseHeader; ABody: TStream);
begin
  PutLogMessage('Response: ' + IntToStr(AStatusCode) + ' ' + AStatusText + ' Length: ' + IntToStr(ABody.Size));
end;

procedure TForm1.HttpsServerReceiveRequest(Sender: TObject;
  AConnection: TclHttpUserConnection; const AMethod, AUri: string;
  AHeader: TclHttpRequestHeader; ABody: TStream);
var
  path: string;
  page: TStrings;
  statusCode: Integer;
  statusText: string;
begin
  PutLogMessage('Request: ' + AMethod + ' ' + AUri + ' Length: ' + IntToStr(ABody.Size));

  AConnection.ResponseHeader.ContentType := 'text/html';

  path := RemoveTrailingBackSlash(edtWebsiteRootDir.Text);

  if (AUri = '/') then
  begin
    path := path + '\default.htm';
  end else
  begin
    path := path + AUri;
  end;

  path := StringReplace(path, '/', '\', [rfReplaceAll]);

  page := TStringList.Create();
  try
    if FileExists(path) then
    begin
      statusCode := 200;
      statusText := 'OK';

      page.LoadFromFile(path);
    end else
    begin
      statusCode := 404;
      statusText := 'Not found';

      page.Add('<html><body>');
      page.Add('<h1>404 - Resource not found</h1>');
      page.Add('<p>The requested resource "' + AUri + '" does not exist.</p>');
      page.Add('</body></html>');
    end;

    (Sender as TclHttpServer).SendResponse(AConnection, statusCode, statusText, page.Text);
  finally
    page.Free();
  end;
end;

end.
