{
  Clever Internet Suite
  Copyright (C) 2017 Clever Components
  All Rights Reserved
  www.CleverComponents.com
}

unit clHttpServer;

interface

uses
  System.Classes, System.SysUtils, clTcpServer, clHttpHeader, clHttpUtils, clUtils, clTranslator;

type
  EclHttpServerError = class(EclTcpServerError)
  end;

  TclHttpUserConnection = class(TclUserConnection)
  private
    FRawRequest: TMemoryStream;
    FRequestVersion: TclHttpVersion;
    FRequestMethod: string;
    FResponseVersion: TclHttpVersion;
    FRequestHeader: TclHttpRequestHeader;
    FResponseHeader: TclHttpResponseHeader;
    FRequestUri: string;
    FRequestCookies: TStrings;
    FResponseCookies: TStrings;
    FRequestBody: TStream;
    FCharSet: string;
    FRequestHeaderReceived: Boolean;

    function ParseHeader(ARawData: TMemoryStream): Boolean;
    procedure ParseRequestLine(const ARequestLine: string);
    procedure ParseCookies(ARequestHeader: TStrings);
    procedure SetRequestBody(const Value: TStream);
  public
    constructor Create(AResponseVersion: TclHttpVersion; const ACharSet: string);
    destructor Destroy; override;

    procedure Clear; virtual;
    function AcceptRequestData(AData: TStream): Boolean;
    function BuildStatusLine(AStatusCode: Integer; const AStatusText: string): string;
    function BuildKeepConnection(AKeepConnection: Boolean): string;

    procedure WriteString(const AString, ACharSet: string);

    property CharSet: string read FCharSet write FCharSet;

    property RequestVersion: TclHttpVersion read FRequestVersion;
    property RequestMethod: string read FRequestMethod;
    property RequestUri: string read FRequestUri;
    property RequestHeader: TclHttpRequestHeader read FRequestHeader;
    property RequestCookies: TStrings read FRequestCookies;

    property ResponseVersion: TclHttpVersion read FResponseVersion write FResponseVersion;
    property ResponseHeader: TclHttpResponseHeader read FResponseHeader;
    property ResponseCookies: TStrings read FResponseCookies;

    property RequestBody: TStream read FRequestBody write SetRequestBody;
  end;

  TclHttpRequestEvent = procedure (Sender: TObject; AConnection: TclHttpUserConnection;
    const AMethod, AUri: string; AHeader: TclHttpRequestHeader; ABody: TStream) of object;

  TclHttpResponseEvent = procedure (Sender: TObject; AConnection: TclHttpUserConnection;
    AStatusCode: Integer; const AStatusText: string; AHeader: TclHttpResponseHeader; ABody: TStream) of object;

  TclHttpServer = class(TclTcpServer)
  private
    FOnSendResponse: TclHttpResponseEvent;
    FOnReceiveRequest: TclHttpRequestEvent;
    FCharSet: string;
    FHttpVersion: TclHttpVersion;

    procedure SendResponseHeader(AConnection: TclHttpUserConnection;
      AStatusCode: Integer; const AStatusText: string; ABody: TStream);
  protected
    function CreateDefaultConnection: TclUserConnection; override;
    procedure DoReadConnection(AConnection: TclUserConnection; AData: TStream); override;

    procedure ProcessRequest(AConnection: TclHttpUserConnection; ARequest: TStream); virtual;
    procedure DoReceiveRequest(AConnection: TclHttpUserConnection;
      const AMethod, AUri: string; AHeader: TclHttpRequestHeader; ABody: TStream); virtual;
    procedure DoSendResponse(AConnection: TclHttpUserConnection;
      AStatusCode: Integer; const AStatusText: string; AHeader: TclHttpResponseHeader; ABody: TStream); virtual;
  public
    constructor Create(AOwner: TComponent); override;

    procedure SendResponse(AConnection: TclHttpUserConnection;
      AStatusCode: Integer; const AStatusText: string; ABody: TStream); overload;
    procedure SendResponse(AConnection: TclHttpUserConnection;
      AStatusCode: Integer; const AStatusText: string; ABody: TStrings); overload;
    procedure SendResponse(AConnection: TclHttpUserConnection;
      AStatusCode: Integer; const AStatusText, ABody: string); overload;

    procedure SendResponseAndClose(AConnection: TclHttpUserConnection;
      AStatusCode: Integer; const AStatusText: string; ABody: TStream); overload;
    procedure SendResponseAndClose(AConnection: TclHttpUserConnection;
      AStatusCode: Integer; const AStatusText: string; ABody: TStrings); overload;
    procedure SendResponseAndClose(AConnection: TclHttpUserConnection;
      AStatusCode: Integer; const AStatusText, ABody: string); overload;
  published
    property Port default DefaultHttpPort;

    property HttpVersion: TclHttpVersion read FHttpVersion write FHttpVersion default hvHttp1_1;
    property CharSet: string read FCharSet write FCharSet;

    property OnReceiveRequest: TclHttpRequestEvent read FOnReceiveRequest write FOnReceiveRequest;
    property OnSendResponse: TclHttpResponseEvent read FOnSendResponse write FOnSendResponse;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Clever Internet Suite', [TclHttpServer]);
end;

{ TclHttpServer }

constructor TclHttpServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FHttpVersion := hvHttp1_1;
  FCharSet := '';

  ServerName := 'Clever Internet Suite HTTP service';
end;

function TclHttpServer.CreateDefaultConnection: TclUserConnection;
begin
  Result := TclHttpUserConnection.Create(HttpVersion, CharSet);
end;

procedure TclHttpServer.DoReadConnection(AConnection: TclUserConnection; AData: TStream);
var
  conn: TclHttpUserConnection;
begin
  inherited DoReadConnection(AConnection, AData);

  conn := TclHttpUserConnection(AConnection);
  try
    ProcessRequest(conn, AData);
  except
    on E: EclHttpServerError do
    begin
      SendResponseAndClose(conn, E.ErrorCode, E.Message, E.Message);
    end;
    on E: EclTcpServerError do
    begin
      SendResponseAndClose(conn, 500, 'Internal Server Error', E.Message);
      raise;
    end;
    on EAbort do ;
    on E: Exception do
    begin
      SendResponseAndClose(conn, 500, 'Internal Server Error', E.Message);
      raise;
    end;
  end;
end;

procedure TclHttpServer.DoReceiveRequest(AConnection: TclHttpUserConnection;
  const AMethod, AUri: string; AHeader: TclHttpRequestHeader; ABody: TStream);
begin
  if Assigned(OnReceiveRequest) then
  begin
    OnReceiveRequest(Self, AConnection, AMethod, AUri, AHeader, ABody);
  end;
end;

procedure TclHttpServer.DoSendResponse(AConnection: TclHttpUserConnection;
  AStatusCode: Integer; const AStatusText: string; AHeader: TclHttpResponseHeader; ABody: TStream);
begin
  if Assigned(OnSendResponse) then
  begin
    OnSendResponse(Self, AConnection, AStatusCode,  AStatusText, AHeader, ABody);
  end;
end;

procedure TclHttpServer.ProcessRequest(AConnection: TclHttpUserConnection; ARequest: TStream);
begin
  if AConnection.AcceptRequestData(ARequest) then
  begin
    DoReceiveRequest(AConnection, AConnection.RequestMethod, AConnection.RequestUri,
      AConnection.RequestHeader, AConnection.RequestBody);
  end;
end;

procedure TclHttpServer.SendResponse(AConnection: TclHttpUserConnection;
  AStatusCode: Integer; const AStatusText: string; ABody: TStream);
begin
  try
    SendResponseHeader(AConnection, AStatusCode, AStatusText, ABody);
    AConnection.WriteData(ABody);
    DoSendResponse(AConnection, AStatusCode, AStatusText, AConnection.ResponseHeader, ABody);
  finally
    AConnection.Clear();
  end;
end;

procedure TclHttpServer.SendResponse(AConnection: TclHttpUserConnection;
  AStatusCode: Integer; const AStatusText: string; ABody: TStrings);
var
  stream: TStream;
begin
  stream := TMemoryStream.Create();
  try
    TclStringsUtils.SaveStrings(ABody, stream, CharSet);
    stream.Position := 0;
    SendResponse(AConnection, AStatusCode, AStatusText, stream);
  finally
    stream.Free();
  end;
end;

procedure TclHttpServer.SendResponse(AConnection: TclHttpUserConnection;
  AStatusCode: Integer; const AStatusText, ABody: string);
var
  stream: TStream;
  buffer: TclByteArray;
begin
  stream := TMemoryStream.Create();
  try
    if (ABody <> '') then
    begin
      buffer := TclTranslator.GetBytes(ABody, CharSet);
      stream.WriteBuffer(buffer[0], Length(buffer));
      stream.Position := 0;
    end;

    SendResponse(AConnection, AStatusCode, AStatusText, stream);
  finally
    stream.Free();
  end;
end;

procedure TclHttpServer.SendResponseAndClose(AConnection: TclHttpUserConnection;
  AStatusCode: Integer; const AStatusText: string; ABody: TStrings);
var
  stream: TStream;
begin
  stream := TMemoryStream.Create();
  try
    TclStringsUtils.SaveStrings(ABody, stream, CharSet);
    stream.Position := 0;
    SendResponseAndClose(AConnection, AStatusCode, AStatusText, stream);
  finally
    stream.Free();
  end;
end;

procedure TclHttpServer.SendResponseAndClose(AConnection: TclHttpUserConnection;
  AStatusCode: Integer; const AStatusText, ABody: string);
var
  stream: TStream;
  buffer: TclByteArray;
begin
  stream := TMemoryStream.Create();
  try
    if (ABody <> '') then
    begin
      buffer := TclTranslator.GetBytes(ABody, CharSet);
      stream.WriteBuffer(buffer[0], Length(buffer));
      stream.Position := 0;
    end;

    SendResponseAndClose(AConnection, AStatusCode, AStatusText, stream);
  finally
    stream.Free();
  end;
end;

procedure TclHttpServer.SendResponseHeader(AConnection: TclHttpUserConnection;
  AStatusCode: Integer; const AStatusText: string; ABody: TStream);
var
  head: TStrings;
begin
  head := TStringList.Create();
  try
    head.Add(AConnection.BuildStatusLine(AStatusCode, AStatusText));

    AConnection.ResponseHeader.ContentLength := IntToStr(ABody.Size);

    AConnection.ResponseHeader.Connection := AConnection.BuildKeepConnection(True);

    AConnection.ResponseHeader.Server := ServerName;

    AConnection.ResponseHeader.AssignHeader(head);

    head.AddStrings(AConnection.ResponseCookies);

    head.Add('');

    AConnection.WriteString(head.Text, 'us-ascii');
  finally
    head.Free();
  end;
end;

procedure TclHttpServer.SendResponseAndClose(AConnection: TclHttpUserConnection;
  AStatusCode: Integer; const AStatusText: string; ABody: TStream);
begin
  try
    SendResponseHeader(AConnection, AStatusCode, AStatusText, ABody);
    AConnection.WriteDataAndClose(ABody);
    DoSendResponse(AConnection, AStatusCode, AStatusText, AConnection.ResponseHeader, ABody);
  finally
    AConnection.Clear();
  end;
end;

{ TclHttpUserConnection }

function TclHttpUserConnection.AcceptRequestData(AData: TStream): Boolean;
var
  cnt: Int64;
begin
  if (FRequestHeaderReceived) then
  begin
    RequestBody.Seek(0, soEnd);
    RequestBody.CopyFrom(AData, 0);
  end else
  begin
    FRawRequest.Seek(0, soEnd);
    FRawRequest.CopyFrom(AData, 0);
    FRawRequest.Seek(0, soBeginning);

    FRequestHeaderReceived := ParseHeader(FRawRequest);
    if (FRequestHeaderReceived) then
    begin
      cnt := FRawRequest.Size - FRawRequest.Position;
      if (cnt > 0) then
      begin
        RequestBody.CopyFrom(FRawRequest, cnt);
      end;
    end;
  end;

  Result := FRequestHeaderReceived
    and (RequestBody.Size >= StrToInt64Def(RequestHeader.ContentLength, 0));
end;

function TclHttpUserConnection.BuildKeepConnection(AKeepConnection: Boolean): string;
begin
  Result := '';

  case ResponseVersion of
    hvHttp1_0:
      begin
        if (AKeepConnection) then
        begin
          Result := 'Keep-Alive';
        end;
      end;
    hvHttp1_1:
      begin
        if (not AKeepConnection) then
        begin
          Result := 'close';
        end;
      end;
  end;
end;

function TclHttpUserConnection.BuildStatusLine(AStatusCode: Integer; const AStatusText: string): string;
begin
  Result := cHttpVersion[ResponseVersion] + ' ' + IntToStr(AStatusCode) + ' ' + AStatusText;
end;

procedure TclHttpUserConnection.Clear;
begin
  FRequestHeaderReceived := False;

  FRawRequest.Size := 0;
  FRequestBody.Size := 0;

  FRequestHeader.Clear();
  FResponseHeader.Clear();
  FRequestCookies.Clear();
  FResponseCookies.Clear();

  FRequestMethod := '';
  FRequestUri := '';
  FRequestVersion := hvHttp1_1;
end;

constructor TclHttpUserConnection.Create(AResponseVersion: TclHttpVersion; const ACharSet: string);
begin
  inherited Create();

  FRawRequest := TMemoryStream.Create();
  FRequestBody := TMemoryStream.Create();

  FRequestHeader := TclHttpRequestHeader.Create();
  FResponseHeader := TclHttpResponseHeader.Create();
  FRequestCookies := TStringList.Create();
  FResponseCookies := TStringList.Create();

  Clear();

  FCharSet := ACharSet;
  FResponseVersion := AResponseVersion;
end;

destructor TclHttpUserConnection.Destroy;
begin
  FResponseCookies.Free();
  FRequestCookies.Free();
  FResponseHeader.Free();
  FRequestHeader.Free();

  FRequestBody.Free();
  FRawRequest.Free();

  inherited Destroy();
end;

procedure TclHttpUserConnection.ParseCookies(ARequestHeader: TStrings);
var
  i: Integer;
begin
  RequestCookies.Clear();

  for i := 0 to ARequestHeader.Count - 1 do
  begin
    if (system.Pos('Cookie', ARequestHeader[i]) = 1) then
    begin
      RequestCookies.Add(ARequestHeader[i]);
    end;
  end;
end;

function TclHttpUserConnection.ParseHeader(ARawData: TMemoryStream): Boolean;

  procedure LoadTextFromStream(AStream: TStream; ACount: Integer; AList: TStrings);
  var
    buf: TclByteArray;
  begin
    SetLength(buf, ACount);
    AStream.Read(buf[0], Length(buf));
    AList.Text := TclTranslator.GetString(buf, CharSet);
  end;

const
  EndOfHeader = #13#10#13#10;
  EndOfHeader2 = #10#10;

var
  ind, eofLength: Integer;
  head: TStrings;
begin
  ind := GetBinTextPos(EndOfHeader, ARawData.Memory, 0, ARawData.Size);
  eofLength := Length(EndOfHeader);
  if (ind < 0) then
  begin
    ind := GetBinTextPos(EndOfHeader2, ARawData.Memory, 0, ARawData.Size);
    eofLength := Length(EndOfHeader2);
  end;

  Result := (ind > 0);
  if not Result then Exit;

  head := TStringList.Create();
  try
    LoadTextFromStream(ARawData, ind + eofLength, head);

    if (head.Count > 0) then
    begin
      ParseRequestLine(head[0]);
      head.Delete(0);
    end;

    RequestHeader.ParseHeader(head);
    ParseCookies(head);
  finally
    head.Free();
  end;
end;

procedure TclHttpUserConnection.ParseRequestLine(const ARequestLine: string);
var
  s: string;
begin
  if (WordCount(ARequestLine, [' ']) <> 3) then
  begin
    raise EclHttpServerError.Create('Bad Request', 400);
  end;

  FRequestMethod := ExtractWord(1, ARequestLine, [' ']);
  FRequestUri := ExtractWord(2, ARequestLine, [' ']);

  s := ExtractWord(3, ARequestLine, [' ']);
  if (cHttpVersion[hvHttp1_1] = s) then
  begin
    FRequestVersion := hvHttp1_1;
  end else
  begin
    FRequestVersion := hvHttp1_0;
  end;
end;

procedure TclHttpUserConnection.SetRequestBody(const Value: TStream);
begin
  FRequestBody.Free();
  FRequestBody := Value;
end;

procedure TclHttpUserConnection.WriteString(const AString, ACharSet: string);
var
  data: TStream;
  buffer: TclByteArray;
begin
  data := TMemoryStream.Create();
  try
    if (AString <> '') then
    begin
      buffer := TclTranslator.GetBytes(AString, ACharSet);
      data.WriteBuffer(buffer[0], Length(buffer));
      data.Position := 0;
    end;
    WriteData(data);
    Assert(data.Position >= (data.Size - 1));
  finally
    data.Free();
  end;
end;

end.
