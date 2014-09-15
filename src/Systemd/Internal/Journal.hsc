{-# LANGUAGE CPP #-}
module Systemd.Internal.Journal where

import Foreign
import Foreign.C

import System.Posix.Types.Iovec (CIovec)

#include <systemd/sd-journal.h>

data Journal

newtype JournalOpenFlag = JournalOpenFlag { _unJournalOpenFlag :: Int32 }
  deriving (Eq, Show)

#{ enum JournalOpenFlag, JournalOpenFlag
 , journalLocalOnly = SD_JOURNAL_LOCAL_ONLY
 , journalRuntimeOnly = SD_JOURNAL_RUNTIME_ONLY
 , journalSystem = SD_JOURNAL_SYSTEM
 , journalCurrentUser = SD_JOURNAL_CURRENT_USER
 }

newtype JournalWakeFlag = JournalWakeFlag { _unJournalWakeFlag :: Int32 }
  deriving (Eq, Show)

#{ enum JournalWakeFlag, JournalWakeFlag
 , journalNop = SD_JOURNAL_NOP
 , journalAppend = SD_JOURNAL_APPEND
 , journalInvalidate = SD_JOURNAL_INVALIDATE
 }

----------------------
-- Function Imports --
----------------------

foreign import ccall unsafe "sd_journal_sendv"
  journalSendV :: Ptr CIovec
               -> Int32
               -> IO Int32

foreign import ccall unsafe "sd_journal_perror"
  journalPError :: CString
                -> IO Int32

foreign import ccall unsafe "sd_journal_stream_fd"
  journalStreamFd :: CString
                  -> Int32
                  -> Int32
                  -> IO Int32

foreign import ccall unsafe "sd_journal_open"
  journalOpen :: Ptr (Ptr Journal)
              -> Int32
              -> IO Int32

foreign import ccall unsafe "sd_journal_close"
  journalClose :: Ptr Journal
               -> IO ()

foreign import ccall unsafe "sd_journal_previous"
  journalPrevious :: Ptr Journal
                  -> IO Int32

foreign import ccall unsafe "sd_journal_next"
  journalNext :: Ptr Journal
              -> IO Int32

foreign import ccall unsafe "sd_journal_previous_skip"
  journalPreviousSkip :: Ptr Journal
                      -> Word64
                      -> IO Int32

foreign import ccall unsafe "sd_journal_next_skip"
  journalNextSkip :: Ptr Journal
                  -> Word64
                  -> IO Int32

foreign import ccall unsafe "sd_journal_get_realtime_usec"
  journalGetRealtimeUsec :: Ptr Journal
                         -> Ptr Word64
                         -> IO Int32

{-

foreign import ccall unsafe "sd_journal_get_monotonic_usec"
  journalGetMonotonicUsec :: Ptr Journal
                         -> Ptr {#type uint64_t}
                         -> Ptr {#type sd_id128_t}
                         -> IO {#type int}

-}

foreign import ccall unsafe "sd_journal_set_data_threshold"
  journalSetDataThreshold :: Ptr Journal
                          -> CSize
                          -> IO Int32

foreign import ccall unsafe "sd_journal_get_data_threshold"
  journalGetDataThreshold :: Ptr Journal
                          -> Ptr CSize
                          -> IO Int32

foreign import ccall unsafe "sd_journal_get_data"
  journalGetData :: Ptr Journal
                 -> CString
                 -> Ptr (Ptr a)
                 -> Ptr CSize
                 -> IO Int32

foreign import ccall unsafe "sd_journal_enumerate_data"
  journalEnumerateData :: Ptr Journal
                       -> Ptr (Ptr a)
                       -> Ptr CSize
                       -> IO Int32

foreign import ccall unsafe "sd_journal_restart_data"
  journalRestartData :: Ptr Journal
                     -> IO Int32

foreign import ccall unsafe "sd_journal_process"
  journalProcess :: Ptr Journal
                 -> IO Int32

foreign import ccall unsafe "sd_journal_wait"
  journalWait :: Ptr Journal
              -> Word64
              -> IO Int32

foreign import ccall unsafe "sd_journal_seek_head"
  journalSeekHead :: Ptr Journal
                  -> IO Int32

foreign import ccall unsafe "sd_journal_seek_tail"
  journalSeekTail :: Ptr Journal
                  -> IO Int32

{-

foreign import ccall unsafe "sd_journal_seek_monotonic_usec"
  journalSeekMonotonicUsec :: Ptr Journal
                           -> Ptr {#type sd_id128_t}
                           -> Word64
                           -> IO Int32

-}

foreign import ccall unsafe "sd_journal_seek_realtime_usec"
  journalSeekRealtimeUsec :: Ptr Journal
                          -> Word64
                          -> IO Int32

foreign import ccall unsafe "sd_journal_seek_cursor"
  journalSeekCursor :: Ptr Journal
                    -> CString
                    -> IO Int32

--------------------
-- FunPtr imports --
--------------------

foreign import ccall unsafe "&sd_journal_sendv"
  journalSendVPtr :: FunPtr 
                   ( Ptr CIovec
                  -> Int32
                  -> IO Int32
                   )

foreign import ccall unsafe "&sd_journal_perror"
  journalPErrorPtr :: FunPtr
                    ( CString
                   -> IO Int32
                    )

foreign import ccall unsafe "&sd_journal_stream_fd"
  journalStreamFdPtr :: FunPtr
                      ( CString
                     -> Int32
                     -> Int32
                     -> IO Int32
                      )

foreign import ccall unsafe "&sd_journal_open"
  journalOpenPtr :: FunPtr
                  ( Ptr (Ptr Journal)
                 -> Int32
                 -> IO Int32
                  )

foreign import ccall unsafe "&sd_journal_close"
  journalClosePtr :: FunPtr
                   ( Ptr Journal
                  -> IO ()
                   )

foreign import ccall unsafe "&sd_journal_previous"
  journalPreviousPtr :: FunPtr
                      ( Ptr Journal
                     -> IO Int32
                      )

foreign import ccall unsafe "&sd_journal_next"
  journalNextPtr :: FunPtr
                  ( Ptr Journal
                 -> IO Int32
                  )

foreign import ccall unsafe "&sd_journal_previous_skip"
  journalPreviousSkipPtr :: FunPtr
                          ( Ptr Journal
                         -> Word64
                         -> IO Int32
                          )

foreign import ccall unsafe "&sd_journal_next_skip"
  journalNextSkipPtr :: FunPtr
                      ( Ptr Journal
                     -> Word64
                     -> IO Int32
                      )

foreign import ccall unsafe "&sd_journal_get_realtime_usec"
  journalGetRealtimeUsecPtr :: FunPtr
                             ( Ptr Journal
                            -> Ptr Word64
                            -> IO Int32
                             )
{-

foreign import ccall unsafe "&sd_journal_get_monotonic_usec"
  journalGetMonotonicUsecPtr :: FunPtr
                              ( Ptr Journal
                             -> Ptr {#type uint64_t}
                             -> Ptr {#type sd_id128_t}
                             -> IO {#type int}
                              )

-}

foreign import ccall unsafe "&sd_journal_set_data_threshold"
  journalSetDataThresholdPtr :: FunPtr
                              ( Ptr Journal
                             -> CSize
                             -> IO Int32
                              )
foreign import ccall unsafe "&sd_journal_get_data_threshold"
  journalGetDataThresholdPtr :: FunPtr
                              ( Ptr Journal
                             -> Ptr CSize
                             -> IO Int32
                              )

foreign import ccall unsafe "&sd_journal_get_data"
  journalGetDataPtr :: FunPtr
                     ( Ptr Journal
                    -> CString
                    -> Ptr (Ptr a)
                    -> Ptr CSize
                    -> IO Int32
                     )

foreign import ccall unsafe "&sd_journal_enumerate_data"
  journalEnumerateDataPtr :: FunPtr
                           ( Ptr Journal
                          -> Ptr (Ptr a)
                          -> Ptr CSize
                          -> IO Int32
                           )

foreign import ccall unsafe "&sd_journal_restart_data"
  journalRestartDataPtr :: FunPtr
                         ( Ptr Journal
                        -> IO Int32
                         )

foreign import ccall unsafe "&sd_journal_process"
  journalProcessPtr :: FunPtr
                     ( Ptr Journal
                    -> IO Int32
                     )

foreign import ccall unsafe "&sd_journal_wait"
  journalWaitPtr :: FunPtr
                  ( Ptr Journal
                 -> Word64
                 -> IO Int32
                  )

foreign import ccall unsafe "&sd_journal_seek_head"
  journalSeekHeadPtr :: FunPtr
                      ( Ptr Journal
                     -> IO Int32
                      )

foreign import ccall unsafe "&sd_journal_seek_tail"
  journalSeekTailPtr :: FunPtr
                      ( Ptr Journal
                     -> IO Int32
                      )

{-

foreign import ccall unsafe "&sd_journal_seek_monotonic_usec"
  journalSeekMonotonicUsecPtr :: FunPtr
                               ( Ptr Journal
                              -> Ptr {#type sd_id128_t}
                              -> Word64
                              -> IO Int32
                               )

-}

foreign import ccall unsafe "&sd_journal_seek_realtime_usec"
  journalSeekRealtimeUsecPtr :: FunPtr
                              ( Ptr Journal
                             -> Word64
                             -> IO Int32
                              )

foreign import ccall unsafe "&sd_journal_seek_cursor"
  journalSeekCursorPtr :: FunPtr
                        ( Ptr Journal
                       -> CString
                       -> IO Int32
                        )
