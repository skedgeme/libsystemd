module Systemd.Wrapper.Journal where

import qualified Systemd.Internal.Journal as I

import Control.Applicative ()
import Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as MV
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import Data.Monoid

import Foreign
import Foreign.C

-- Todo: Wrap in MVar as well as ForeignPtr
data Journal = Journal { _journalHandle :: MVar (ForeignPtr I.Journal)
                       }

newtype JournalCursor = JournalCursor { _unJournalCursor :: ByteString }
  deriving (Show) -- Comparing cursors is not semantically valid.
                  -- Journal cursors are null-terminated printable strings.

newtype JournalField = JournalField { _unJournalField :: ByteString }
  deriving (Eq, Ord, Show, Read)

data JournalSeekResult = JournalSeekResult { _journalSeekResultRequested :: Word64
                                           , _journalSeekResultReturned :: Int32
                                           }
  deriving (Eq, Ord, Show)

-------------------------------
-- Internal Helper Functions --
-------------------------------

withJournalPtr :: Journal -> (Ptr I.Journal -> IO a) -> IO a
withJournalPtr (Journal mv) k = MV.withMVar mv $ \fptr -> withForeignPtr fptr k

withJournalCursor :: JournalCursor -> (CString -> IO a) -> IO a
withJournalCursor (JournalCursor c) = BS.unsafeUseAsCString c

withJournalField :: JournalField -> (CString -> IO a) -> IO a
withJournalField (JournalField s) = BS.unsafeUseAsCString s

journalError :: String -> Int32 -> String
journalError s e = "Error in < " <> s <> " > : " <> show e

----------------
-- Public API --
----------------
data JournalWakeStatus = Nop | Append | Invalidate
  deriving (Show, Eq, Enum)

bitflagToStatusList :: I.JournalWakeFlag -> [JournalWakeStatus]
bitflagToStatusList (I.JournalWakeFlag bits) = nop ++ append ++ invalidate
  where
    nop = flagIf bits I.journalNop Nop
    append = flagIf bits I.journalAppend Append
    invalidate = flagIf bits I.journalInvalidate Invalidate
    flagIf bits (I.JournalWakeFlag flag) t = if bits .&. flag == flag then [t] else []

createJournalHandle :: [I.JournalOpenFlag] -> IO Journal
createJournalHandle flags =
  alloca $ \pptr -> do
    let combinedFlags = foldl1 (.|.) . map I._unJournalOpenFlag $ flags 
    throwIfNeg_ (journalError "createJournalHandle") (I.journalOpen pptr combinedFlags)
    fptr <- newForeignPtr I.journalClosePtr =<< peek pptr
    mv <- MV.newMVar fptr
    return (Journal mv)

journalNext :: Journal -> IO JournalSeekResult
journalNext j = do
  n <- throwIfNeg (journalError "journalNext") $ withJournalPtr j I.journalNext
  return $ JournalSeekResult 1 n

journalPrevious :: Journal -> IO JournalSeekResult
journalPrevious j = do
  n <- throwIfNeg (journalError "journalPrevious") $ withJournalPtr j I.journalPrevious
  return $ JournalSeekResult 1 n

journalNextSkip :: Journal -> Word64 -> IO JournalSeekResult
journalNextSkip j skip = do
  n <- throwIfNeg (journalError "journalNextSkip") $ withJournalPtr j $ \p -> I.journalNextSkip p skip
  return $ JournalSeekResult skip n

journalPreviousSkip :: Journal -> Word64 -> IO JournalSeekResult
journalPreviousSkip j skip = do
  n <- throwIfNeg (journalError "journalPreviousSkip") $ withJournalPtr j $ \p -> I.journalPreviousSkip p skip
  return $ JournalSeekResult skip n

journalGetRealtimeUsec :: Journal -> IO Word64
journalGetRealtimeUsec j = do
  alloca $ \tptr -> do
    throwIfNeg_ (journalError "journalGetRealtimeUsec") $ withJournalPtr j (\p -> I.journalGetRealtimeUsec p tptr)
    peek tptr

journalSetDataThreshold :: Journal -> CSize -> IO ()
journalSetDataThreshold j sz = throwIfNeg_ (journalError "journalSetDataThreshold") $ withJournalPtr j $ \p -> I.journalSetDataThreshold p sz

journalGetDataThreshold :: Journal -> IO CSize
journalGetDataThreshold j = alloca $ \sptr -> do
  throwIfNeg_ (journalError "journalGetDataThreshold") $ withJournalPtr j $ \p -> I.journalGetDataThreshold p sptr
  peek sptr

journalGetData :: Journal -> JournalField -> IO ByteString
journalGetData j f =
  alloca $ \vptr -> alloca $ \sptr -> do
    throwIfNeg_ (journalError "journalGetData") $ withJournalPtr j $ \p -> withJournalField f $ \str -> I.journalGetData p str vptr sptr
    dat <- peek vptr
    sz <- peek sptr
    BS.packCStringLen (dat, fromIntegral sz) 

journalProcess :: Journal -> IO I.JournalWakeFlag
journalProcess j = fmap I.JournalWakeFlag . throwIfNeg (journalError "journalProcess") $ withJournalPtr j I.journalProcess

journalWait :: Journal -> Word64 -> IO I.JournalWakeFlag
journalWait j timeout = fmap I.JournalWakeFlag . throwIfNeg (journalError "journalWait") $ withJournalPtr j $ \p -> I.journalWait p timeout

journalSeekHead :: Journal -> IO ()
journalSeekHead j = throwIfNeg_ (journalError "journalSeekHead") $ withJournalPtr j I.journalSeekHead

journalSeekTail :: Journal -> IO ()
journalSeekTail j = throwIfNeg_ (journalError "journalSeekTail") $ withJournalPtr j I.journalSeekTail

journalSeekRealtimeUsec :: Journal -> Word64 -> IO ()
journalSeekRealtimeUsec j time = throwIfNeg_ (journalError "journalSeekRealtimeUsec") $ withJournalPtr j $ \p -> I.journalSeekRealtimeUsec p time

journalSeekCursor :: Journal -> JournalCursor -> IO ()
journalSeekCursor j cur = throwIfNeg_ (journalError "journalSeekCursor") $ withJournalPtr j $ \p -> withJournalCursor cur $ \c -> I.journalSeekCursor p c

-----------------------
-- Predefined Fields --
-----------------------
message, messageId, priority, codeFile, codeLine, codeFunc, errNo, sysLogFacility, sysLogIdentifier, sysLogPid, pid, uid, gid, comm, exe, cmdLine, capEffective, auditSession, auditLoginUid, systemdCGroup, systemdSession, systemdUnit, systemdUserUnit, systemdOwnerUid, systemdSlice, seLinuxContext, sourceRealtimeTimestamp, bootId, machineId, hostname, transport, kernelDevice, kernelSubsystem, kernelUdevSysname, kernelUdevDevnode, kernelUdevDevlink, coredumpUnit, coredumpUserUnit, objectPid, objectUid, objectGid, objectComm, objectExe, objectCmdLine, objectAuditSession, objectAuditLoginUid, objectSystemdCGroup, objectSystemdSession, objectSystemdUnit, objectSystemdOwnerUid, objectSystemdUserUnit :: JournalField

message = JournalField "MESSAGE"
messageId = JournalField "MESSAGE_ID"
priority = JournalField "PRIORITY"
codeFile = JournalField "CODE_FILE"
codeLine = JournalField "CODE_LINE"
codeFunc = JournalField "CODE_FUNC"
errNo = JournalField "ERRNO"
sysLogFacility = JournalField "SYSLOG_FACILITY"
sysLogIdentifier = JournalField "SYSLOG_IDENTIFIER"
sysLogPid = JournalField "SYSLOG_PID"
pid = JournalField "_PID"
uid = JournalField "_UID"
gid = JournalField "_GID"
comm = JournalField "_COMM"
exe = JournalField "_EXE"
cmdLine = JournalField "_CMDLINE"
capEffective = JournalField "_CAP_EFFECTIVE"
auditSession = JournalField "_AUDIT_SESSION"
auditLoginUid = JournalField "_AUDIT_LOGINUID"
systemdCGroup = JournalField "_SYSTEMD_CGROUP"
systemdSession = JournalField "_SYSTEMD_SESSION"
systemdUnit = JournalField "_SYSTEMD_UNIT"
systemdUserUnit = JournalField "_SYSTEMD_USER_UNIT"
systemdOwnerUid = JournalField "_SYSTEMD_OWNER_UID"
systemdSlice = JournalField "_SYSTEMD_SLICE"
seLinuxContext = JournalField "_SELINUX_CONTEXT"
sourceRealtimeTimestamp = JournalField "_SOURCE_REALTIME_TIMESTAMP"
bootId = JournalField "_BOOT_ID"
machineId = JournalField "_MACHINE_ID"
hostname = JournalField "_HOSTNAME"
transport = JournalField "_TRANSPORT"
kernelDevice = JournalField "_KERNEL_DEVICE"
kernelSubsystem = JournalField "_KERNEL_SUBSYSTEM"
kernelUdevSysname = JournalField "_UDEV_SYSNAME"
kernelUdevDevnode = JournalField "_UDEV_DEVNODE"
kernelUdevDevlink = JournalField "_UDEV_DEVLINK"
coredumpUnit = JournalField "COREDUMP_UNIT"
coredumpUserUnit = JournalField "COREDUMP_USER_UNIT"
objectPid = JournalField "OBJECT_PID"
objectUid = JournalField "OBJECT_UID"
objectGid = JournalField "OBJECT_GID"
objectComm = JournalField "OBJECT_COMM"
objectExe = JournalField "OBJECT_EXE"
objectCmdLine = JournalField "OBJECT_CMDLINE"
objectAuditSession = JournalField "OBJECT_AUDIT_SESSION"
objectAuditLoginUid = JournalField "OBJECT_AUDIT_LOGINUID"
objectSystemdCGroup = JournalField "OBJECT_SYSTEMD_CGROUP"
objectSystemdSession = JournalField "OBJECT_SYSTEMD_SESSION"
objectSystemdUnit = JournalField "OBJECT_SYSTEMD_UNIT"
objectSystemdOwnerUid = JournalField "OBJECT_SYSTEMD_OWNER_UID"
objectSystemdUserUnit = JournalField "OBJECT_SYSTEMD_USER_UNIT"
