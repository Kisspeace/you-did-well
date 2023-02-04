{ ❤ 2022 by Kisspeace - https://github.com/Kisspeace }
{ ❤ Part of you-did-well library ❤ ---------------- }
unit YDW.Threading;

interface
uses
  System.SysUtils, System.Generics.Collections,
  System.Classes, System.SyncObjs, YDW.Debug;

type

  TYDWMREWSync = TMREWSync;

  TYdwReusableThread = class(TObject)
    protected type
      TWorkerThread = Class(TThread)
        public
          Owner: TYdwReusableThread;
          procedure Execute; override;
      End;
    protected
      FThread: TWorkerThread;
      FLock: TYDWMREWSync;
      { ❤ -- Thread safe --- }
      function GetIsRunning: boolean;
      procedure Start;
      procedure Terminate;
      function WaitFor: LongWord;
      { --------------------- }
      function UnlockedIsRunning: boolean;
      procedure UnlockedStart;
      procedure Execute; virtual; abstract;
    public
      constructor Create; virtual;
      destructor Destroy; override;
  end;

  TGenericYDWQueuedThread<T> = Class(TYdwReusableThread)
    protected const
      DEFAULT_THREADS_COUNT: integer = 4;
    protected type
      TSubWorkerThread = Class(TThread)
        public
          Owner: TGenericYDWQueuedThread<T>;
          Value: T;
          procedure Execute; override;
      End;
      TThreadAndValue<T> = record
        public
          Thread: TSubWorkerThread;
          Value: T;
          constructor Create(AValue: T);
      end;
      { ❤-------------- }
      TThreadAndValueList = TList<TThreadAndValue<T>>;
    protected
    { ❤ -- Thread safe --- }
      procedure SetThreadsCount(const value: integer);
      function GetThreadsCount: integer;
      procedure QueueAdd(AItem: T);
      procedure AbortItem(AItem: T);
      function RunningCount: integer;
      function IsRunning(AValue: T): boolean;
      function WaitForItem(AValue: T): LongWord;
      function OnWaitList(AThread: TSubWorkerThread): boolean;
    protected
      FThreadsCount: integer;
      FQueue: TList<T>;
      FRunning: TThreadAndValueList;
      FWaitList: TThreadList;
      function NewSubThread(AValue: T): TSubWorkerThread;
      function QueueCondition: boolean; virtual;
      function AutoRestartCondition: boolean; virtual;
      procedure OnSubThreadFinish; virtual;
      function GetThreadByItem(AValue: T): TSubWorkerThread;
      function RunningIndex(AValue: T): integer; virtual; abstract; { dont forget this ! }
      procedure Execute; override;
      procedure SubThreadExecute(AItem: T); virtual; abstract; { dont forget this ! }
    public
      property ThreadsCount: integer read GetThreadsCount write SetThreadsCount;
      constructor Create; override;
      destructor Destroy; override;
  End;

  TGenericYDWQueuedThreadObject<T: class> = Class(TGenericYDWQueuedThread<T>)
    protected
      function RunningIndex(AValue: T): integer; override;
  End;

  TGenericYDWQueuedThreadInterface<T: IInterface> = Class(TGenericYDWQueuedThread<T>)
    protected
      function RunningIndex(AValue: T): integer; override;
  End;


implementation

{ TYdwQueuedThreadComponent }

function TGenericYDWQueuedThread<T>.AutoRestartCondition: boolean;
begin
  Result := ( Self.QueueCondition ) and
            ( not TThread.Current.CheckTerminated);
end;

procedure TGenericYDWQueuedThread<T>.Execute;
const
  MICRO_SLEEP_TIME = 10;
var
  I: integer;
  LNewThread: TSubWorkerThread;
  LItem: TThreadAndValue<T>;
begin
  try
    repeat
      try

        while ( TRUE ) do begin

          if TThread.Current.CheckTerminated then exit;

          While ( RunningCount >= Self.ThreadsCount ) do begin
            if TThread.Current.CheckTerminated then exit;
            Sleep(MICRO_SLEEP_TIME);
          end;

          FLock.BeginWrite();
          try
            if ( not Self.QueueCondition ) then
              break;

            LItem := TThreadAndValue<T>.Create(FQueue.First); { get next Value from queue }
            LNewThread := NewSubThread(LItem.Value);
            LItem.Thread := LNewThread;
            FRunning.Add(LItem);
            FQueue.Delete(0);
            LNewThread.Start;

            while not LNewThread.Started do
              Sleep(1);

          finally
            FLock.EndWrite();
          end;

        end;

        { waiting for end }
        while ( RunningCount > 0 ) do begin
          if TThread.Current.CheckTerminated then exit;
          sleep(MICRO_SLEEP_TIME);
        end;

      finally

        { Terminate all threads }
        FLock.BeginWrite();
        try

          for I := 0 to FRunning.count - 1 do begin
            var LThread := FRunning[I].Thread;

            LThread.Terminate;

          end;

        finally
          FLock.EndWrite();
        end;

        { Waiting for finish }
        while (RunningCount > 0) do begin
          sleep(MICRO_SLEEP_TIME);
        end;
        
        var LListEmpty: boolean := False;
        while (not LListEmpty) do begin
          var LWaitList := FWaitList.LockList;
          try
            LListEmpty := (LWaitList.Count = 0) 
          finally
            FWaitList.UnlockList;
          end;
        end;
        
      end;
    until not AutoRestartCondition;
  except

    on E: Exception do begin
      Raise E;
    end;

  end;
end;

function TGenericYDWQueuedThread<T>.GetThreadByItem(
  AValue: T): TSubWorkerThread;
var
  LIndex: integer;
begin
  LIndex := Self.RunningIndex(AValue);
  if (LIndex <> -1) then
    Result := FRunning[LIndex].Thread
  else
    Result := NIL;
end;

function TGenericYDWQueuedThread<T>.GetThreadsCount: integer;
begin
  FLock.BeginRead();
  Result := FThreadsCount;
  FLock.EndRead();
end;

function TGenericYDWQueuedThread<T>.IsRunning(AValue: T): boolean;
begin
  FLock.BeginRead();
  try
    Result := (Self.RunningIndex(AValue) <> -1);
  finally
    FLock.EndRead();
  end;
end;

function TGenericYDWQueuedThread<T>.NewSubThread(AValue: T): TSubWorkerThread;
begin
  Result := TSubWorkerThread.Create(True);
  Result.Owner := Self;
  Result.Value := AValue;
  Result.FreeOnTerminate := False;
end;

function TGenericYDWQueuedThread<T>.RunningCount: integer;
begin
  FLock.BeginRead();
  Result := Self.FRunning.Count;
  FLock.EndRead();
end;

procedure TGenericYDWQueuedThread<T>.SetThreadsCount(const Value: integer);
begin
  FLock.BeginWrite();
  try
    if (Value > 0) then
      FThreadsCount := value;
  finally
    FLock.EndWrite();
  end;
end;

function TGenericYDWQueuedThread<T>.WaitForItem(AValue: T): LongWord;
var
  LThread: TSubWorkerThread;
  LIndex: integer;
begin
  {$IFDEF YDW_DEBUG} try {$ENDIF}
    FLock.BeginRead;
    try
      LThread := GetThreadByItem(AValue);
      if Assigned(LThread) then
        FWaitList.Add(LThread);
    finally
      FLock.EndRead;
    end;

    if Assigned(LThread) then
      Result := LThread.WaitFor;

    FWaitList.Remove(LThread);
  {$IFDEF YDW_DEBUG} except
    On E: Exception do begin
      YDW.Debug.Log('TGenericYDWQueuedThread<T>.WaitForItem', E);
      raise E;
    end;
  end; {$ENDIF}
end;

{ TYdwReusableThread }

constructor TYdwReusableThread.Create;
begin
  FLock := TYDWMREWSync.Create;
  FThread := Nil;
end;

destructor TYdwReusableThread.Destroy;
begin
  self.Terminate;
  self.WaitFor;
  FLock.Free;
  inherited;
end;

function TYdwReusableThread.GetIsRunning: boolean;
begin
  FLock.BeginRead();
  try
    Result := Self.UnlockedIsRunning;
  finally
    FLock.EndRead();
  end;
end;

procedure TYdwReusableThread.Start;
begin
  FLock.BeginWrite();
  try
    Self.UnlockedStart;
  finally
    FLock.EndWrite();
  end;
end;

procedure TYdwReusableThread.Terminate;
begin
  {$IFDEF YDW_DEBUG} try {$ENDIF}
    FLock.BeginWrite();
    try
      if Assigned(FThread) then
        FThread.Terminate;
    finally
      FLock.EndWrite();
    end;
  {$IFDEF YDW_DEBUG} except
    On E: Exception do begin
      YDW.Debug.Log('TYdwReusableThread.Terminate', E);
      Raise E;
    end;
  end; {$ENDIF}
end;

function TYdwReusableThread.UnlockedIsRunning: boolean;
begin
  {$IFDEF YDW_DEBUG} try {$ENDIF}
    Result := Assigned(FThread);
    if Result then begin
      Result := not FThread.Finished;
    end;
  {$IFDEF YDW_DEBUG} except
    On E: Exception do begin
      YDW.Debug.Log('TYdwReusableThread.UnlockedIsRunning', E);
      Raise E;
    end;
  end; {$ENDIF}
end;

procedure TYdwReusableThread.UnlockedStart;
begin
  FThread := TWorkerThread.Create(True);
  FThread.Owner := Self;
  FThread.FreeOnTerminate := False;
  FThread.Start;

  While not FThread.Started do
    Sleep(1);
end;

function TYdwReusableThread.WaitFor: LongWord;
begin
  {$IFDEF YDW_DEBUG} try {$ENDIF}
    if Self.GetIsRunning then begin
      Result := FThread.WaitFor;
    end;
  {$IFDEF YDW_DEBUG} except
    On E: exception do begin
      YDW.Debug.Log('TYdwReusableThread.WaitForFinish ' + FThread.ThreadID.ToString, E);
      Raise E;
    end;
  end; {$ENDIF}
end;

{ TYdwQueuedThreadComponent.TThreadAndValue<T> }

constructor TGenericYDWQueuedThread<T>.TThreadAndValue<T>.Create(AValue: T);
begin
  Self.Thread := Nil;
  Self.Value := AValue;
end;

{ TGenericYdwQueuedThreadComponent<T> }

procedure TGenericYDWQueuedThread<T>.AbortItem(AItem: T);
var
  Index: integer;
begin
  FLock.BeginWrite();
  try
    Index := Self.RunningIndex(AItem);
    if ( Index <> -1 ) then begin
      try
        FRunning[Index].Thread.Terminate;
      except
        On E: exception do
          raise E;
      end;
    end;

    Index := FQueue.IndexOf(AItem);
    if ( Index <> -1 ) then begin
      FQueue.Delete(Index);
    end;

  finally
    FLock.EndWrite();
  end;
end;

constructor TGenericYDWQueuedThread<T>.Create;
begin
  inherited;
  FQueue := TList<T>.Create;
  FRunning := TThreadAndValueList.Create;
  FThreadsCount := DEFAULT_THREADS_COUNT;
  FWaitList := TThreadList.Create;
  FWaitList.Duplicates := dupAccept;
end;

destructor TGenericYDWQueuedThread<T>.Destroy;
begin
  inherited;
  FQueue.Free;
  FRunning.Free;
  FWaitList.Free;
end;

procedure TGenericYDWQueuedThread<T>.OnSubThreadFinish;
begin
end;

function TGenericYDWQueuedThread<T>.OnWaitList(
  AThread: TSubWorkerThread): boolean;
var
  I: integer;
begin
  var LWaitList := FWaitList.LockList;
  try
    Result := (LWaitList.IndexOf(AThread) <> -1);
  finally
    FWaitList.UnlockList;
  end;
end;

procedure TGenericYDWQueuedThread<T>.QueueAdd(AItem: T);
begin
  FLock.BeginWrite();
  try
    FQueue.Add(AItem);
    if not UnlockedIsRunning then
      UnlockedStart;
  finally
    FLock.EndWrite();
  end;
end;

function TGenericYDWQueuedThread<T>.QueueCondition: boolean;
begin
  Result := FQueue.Count > 0;
end;

{ TObjectYDWQueuedThreadComponent<T> }

function TGenericYDWQueuedThreadObject<T>.RunningIndex(AValue: T): integer;
var
  I: integer;
begin
  for I := 0 to FRunning.Count - 1 do begin
    if ( AValue = FRunning[I].value ) then
    begin
      Result := I;
      exit;
    end;
  end;
  Result := -1;
end;

{ TInterfaceYDWQueuedThreadComponent<T> }

function TGenericYDWQueuedThreadInterface<T>.RunningIndex(AValue: T): integer;
var
  I: integer;
begin
  for I := 0 to FRunning.Count - 1 do begin
    if ((AValue as TObject) = (FRunning[I].value as TObject )) then
    begin
      Result := I;
      exit;
    end;
  end;
  Result := -1;
end;

{ TYdwReusableThread.TWorkerThread }

procedure TYdwReusableThread.TWorkerThread.Execute;
begin
  Owner.Execute;
end;

{ TGenericYDWQueuedThread<T>.TSubWorkerThread }

procedure TGenericYDWQueuedThread<T>.TSubWorkerThread.Execute;
var
  I, ThreadIndex, LastIndex: integer;
  TempItem: TThreadAndValue<T>;
begin
  try
    Owner.SubThreadExecute(Value);

  finally
    Owner.FLock.BeginWrite();
    try
      LastIndex := Owner.FRunning.Count - 1;

      for I := 0 to LastIndex do begin
        if ( Owner.FRunning[I].Thread = TThread.Current ) then
        begin
          ThreadIndex := I;
          Break;
        end;
      end;

      if ( ThreadIndex <> LastIndex ) and ( Owner.FRunning.Count > 1 ) then
      begin
        Owner.FRunning.Exchange(LastIndex, ThreadIndex);
      end;

      Owner.FRunning.Delete(LastIndex);

    finally
      Owner.FLock.EndWrite();
    end;

    Owner.OnSubThreadFinish;
  end;
end;

end.
