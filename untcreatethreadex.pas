unit uNtCreateThreadEx;

{$mode objfpc}{$H+}

interface

uses
  windows, Classes, SysUtils;

function NtCreateThreadEx(process:HANDLE; Start:LPTHREAD_START_ROUTINE; lpParameter:LPVOID; pThreadid:LPDWORD):HANDLE;

implementation

//for VISTA/WIN7
{ 64bit
  struct N065C26D1{
      char buf[64];
  };

  struct NtCreateThreadExBuffer
  {
      __int64 cbSize; //0x0000 => 72(sizeof struct)
      __int64 Unknown; //0x0008 => 65539
      __int64 Unknown2; //0x0010 => 16
      N065C26D1* UnknownPtr; //0x0018 => pointer to zero memory.
      __int64 Unknown3; //0x0020 => zero
      __int64 Unknown4; //0x0028 => 65540
      __int64 Unknown5; //0x0030  => 8
      N065C26D1* UnknownPtr2; //0x0038 => pointer to zero memory(64b).
      __int64 Unknown6; //0x0040 => zero

  };//Size=0x0048
}

type
  TLPFUN_NtCreateThreadEx = function (hTHread:PHANDLE; DesiredAccess:ACCESS_MASK;
                                ObjectAttributes:LPVOID; ProcessHandle:HANDLE;
                                lpStartAddress:LPTHREAD_START_ROUTINE;
                                lpParameter:LPVOID; CreateSuspended:WINBOOL;
                                StackZeroBits, SizeOfStackCommit,
                                SizeOfStackReserve: PtrUInt;
                                lpBytesBuffer:LPVOID):DWORD; stdcall;

  PThreadexId = ^TThreadexId;
  TThreadexId = record
    ProcessId,
    ThreadId,
    dummy1,
    dummy2 : PtrUInt;
  end;

  NtCreateThreadExBuffer = record
    Size,
    Unknown1,
    UnKnown2: PtrUInt;
    pThreadexId: pThreadexId;
    UnKnown4,
    UnKnown5,
    UnKnown6: PtrUInt;
    pPTEB: PPtrUInt;
    UnKnown8: PtrUInt;
  end;

function NtCreateThreadEx(process:HANDLE; Start:LPTHREAD_START_ROUTINE; lpParameter:LPVOID; pThreadid:LPDWORD):HANDLE;
var
  modNtDll:HMODULE;
  funNtCreateThreadEx:TLPFUN_NtCreateThreadEx;
  ntbuffer:NtCreateThreadExBuffer;
  hThread:HANDLE;
  status:DWORD;
  threadExBuf : TThreadexId;
  temp1 : PtrUInt;
begin
  modNtDll:=LoadLibrary('ntdll.dll');
  if modNtDll=0 then
  begin
    Result:=0;
    exit;
  end;
  Pointer(funNtCreateThreadEx):=GetProcAddress(modNtDll, 'NtCreateThreadEx');
  if funNtCreateThreadEx=nil then
  begin
    Result:=0;
    exit;
  end;
  fillchar(ntbuffer,sizeof(NtCreateThreadExBuffer),0);
  ntbuffer.Size := sizeof(NtCreateThreadExBuffer);

  fillchar(threadExBuf,sizeof(TThreadexId),0);
  temp1:=0;
  {$ifdef CPU32}
  // 32bit
  ntbuffer.Unknown1 := $10003;
  ntbuffer.Unknown2 := $8;
  ntbuffer.pThreadexId := @threadexbuf;
  ntbuffer.Unknown4 := 0;
  ntbuffer.Unknown5 := $10004;
  ntbuffer.Unknown6 := 4;
  ntbuffer.pPTEB := @temp1;
  {$else}
  // 64bit
  ntbuffer.Unknown1 := 65539;
  ntbuffer.Unknown2 := $10;
  ntbuffer.pThreadexId := @threadexbuf;
  ntbuffer.Unknown4 := 0;
  ntbuffer.Unknown5 := 65540;
  ntbuffer.Unknown6 := 8;
  ntbuffer.pPTEB := @temp1;
  {$endif}
  status:=funNtCreateThreadEx(@hThread, $1FFFFF, nil, process, Start, lpParameter,
                      False, 0, 0, 0, @ntbuffer);
  Result:=hThread;
  if pThreadid<>nil then
    pThreadid^:=ntbuffer.pThreadexId^.ThreadId;
end;

end.

