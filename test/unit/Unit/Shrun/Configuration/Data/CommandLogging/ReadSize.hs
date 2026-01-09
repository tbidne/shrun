module Unit.Shrun.Configuration.Data.CommandLogging.ReadSize
  ( tests,
  )
where

import Data.Bytes
  ( FloatingFormatter (MkFloatingFormatter),
    MGroup,
    RawNumeric (Raw),
    SomeSize,
  )
import Data.Bytes qualified as Bytes
import Data.Bytes.Formatting (IntegralFormatter (MkIntegralFormatter))
import Data.Bytes.Formatting qualified as BytesFmt
import Data.Bytes.Formatting.Base (BaseFormatter, Formatter)
import Data.Bytes.Size (Size (E, G, K, M, P, T, Y, Z))
import GHC.Real (fromIntegral)
import Hedgehog.Gen qualified as G
import Hedgehog.Range qualified as R
import Shrun.Configuration.Data.CommandLogging.ReadSize qualified as ReadSize
import Text.Printf (PrintfArg)
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Shrun.Configuration.Data.CommandLogging.ReadSize"
    [ parseNatSucceeds,
      parseDoubleSucceeds,
      maxIntSupported,
      maxIntDoubleSupported,
      maxIntIncNotSupported,
      maxIntDoubleIncNotSupported
    ]

-- Properties for all b in 0 <= b <= max int

parseNatSucceeds :: TestTree
parseNatSucceeds = testPropertyNamed desc name $ do
  property $ do
    allNatSizes <- forAll genAllMaxBytes

    for_ allNatSizes $ \bytes -> do
      let txt = bytesToText MkIntegralFormatter bytes
      case ReadSize.parseReadSize (pure txt) of
        Err err -> do
          annotate $ expectedSuccess txt err
          failure
        Ok _ -> pure ()
  where
    desc = "Integral bytes up to max Int succeedes"
    name = "parseNatSucceeds"

parseDoubleSucceeds :: TestTree
parseDoubleSucceeds = testPropertyNamed desc name $ do
  property $ do
    allDoubleSizes <- forAll genAllMaxBytesDouble

    for_ allDoubleSizes $ \bytes -> do
      -- Need at least 6 decimal places here, otherwise may convert to
      -- something too large. For example, say we generate a raw Int of
      --
      --     5_000_000_000_000_001_000
      --
      -- This is less than the 64-bit max of 9_223_372_036_854_775_807, so
      -- it should be fine. The first conversion to Double ends up being
      --
      --     5.000000000000001e18
      --
      -- When converted to size Y, this becomes
      --
      --     5.000000000000001e-6
      --
      -- If converted to 6 decimal places, this is
      --
      --     0.000005Y
      --
      -- On the other hand, converted to 5 decimal places, this is
      --
      --     0.00001Y
      --
      -- That is, it was rounded up, and suddenly it is too large. Hence we
      -- need at least 6 decimal places to store the max Int.
      let txt = bytesToText (MkFloatingFormatter (Just 6)) bytes
      case ReadSize.parseReadSize (pure txt) of
        Err err -> do
          annotate $ expectedSuccess txt err
          failure
        Ok _ -> pure ()
  where
    desc = "Floating bytes up to max Int succeedes"
    name = "parseDoubleSucceeds"

-- Explicit specs at the boundary

maxIntSupported :: TestTree
maxIntSupported = testCase "ReadSize can be Int maxBound" $ do
  case ReadSize.parseReadSize (pure bytesStr) of
    Err err -> assertFailure $ expectedSuccess bytesStr err
    Ok _ -> pure ()
  where
    val = fromIntegral @Int @Natural maxBound
    bytesStr = mkBytesString val

maxIntDoubleSupported :: TestTree
maxIntDoubleSupported = testCase "ReadSize can be Double near max Int" $ do
  case ReadSize.parseReadSize (pure bytesStr) of
    Err err -> assertFailure $ expectedSuccess bytesStr err
    Ok _ -> pure ()
  where
    -- Why - 1000? Because if we convert the max Int it will round up and
    -- be too large.
    val = MkBytes @B $ (fromIntegral @Int @Double maxBound - 1_000)
    bytesStr = bytesToText (MkFloatingFormatter Nothing) val

maxIntIncNotSupported :: TestTree
maxIntIncNotSupported = testCase "ReadSize cannot be Int maxBound + 1" $ do
  case ReadSize.parseReadSize @(Result Text) (pure bytesStr) of
    Err _ -> pure ()
    Ok x -> assertFailure $ expectedFailure bytesStr x
  where
    val = fromIntegral @Int @Natural maxBound + 1
    bytesStr = mkBytesString val

maxIntDoubleIncNotSupported :: TestTree
maxIntDoubleIncNotSupported = testCase desc $ do
  case ReadSize.parseReadSize @(Result Text) (pure bytesStr) of
    Err _ -> pure ()
    Ok x -> assertFailure $ expectedFailure bytesStr x
  where
    desc = "ReadSize cannot be Double at max Int"
    val = MkBytes @B $ fromIntegral @Int @Double maxBound
    bytesStr = bytesToText (MkFloatingFormatter Nothing) val

genAllMaxBytes :: Gen (List (SomeSize Natural))
genAllMaxBytes = bytesToAllSizes <$> genMaxBytes

genAllMaxBytesDouble :: Gen (List (SomeSize Double))
genAllMaxBytesDouble = bytesToAllSizes <$> genMaxBytesDouble

bytesToAllSizes :: (FromInteger a, MGroup a) => Bytes B a -> List (SomeSize a)
bytesToAllSizes b =
  [ Bytes.hideSize b,
    Bytes.hideSize $ Bytes.convert_ @_ @K b,
    Bytes.hideSize $ Bytes.convert_ @_ @M b,
    Bytes.hideSize $ Bytes.convert_ @_ @G b,
    Bytes.hideSize $ Bytes.convert_ @_ @T b,
    Bytes.hideSize $ Bytes.convert_ @_ @P b,
    Bytes.hideSize $ Bytes.convert_ @_ @E b,
    Bytes.hideSize $ Bytes.convert_ @_ @Z b,
    Bytes.hideSize $ Bytes.convert_ @_ @Y b
  ]

genMaxBytes :: Gen (Bytes B Natural)
genMaxBytes = MkBytes <$> genUptoMaxInt

genMaxBytesDouble :: Gen (Bytes B Double)
genMaxBytesDouble = MkBytes . fromIntegral <$> genUptoMaxInt

genUptoMaxInt :: Gen Natural
genUptoMaxInt = G.integral (R.exponentialFrom 0 0 maxIntAsNat)
  where
    maxIntAsNat = fromIntegral @Int @Natural maxBound

bytesToText ::
  ( BaseFormatter (Raw (f a)) ~ fmt,
    Formatter fmt,
    PrintfArg (Raw (f a)),
    RawNumeric (f a),
    Sized (f a)
  ) =>
  fmt ->
  f a ->
  Text
bytesToText fmtter = BytesFmt.formatSized fmtter BytesFmt.sizedFormatterUnix

expectedSuccess :: Text -> Text -> String
expectedSuccess val err =
  unpack
    $ mconcat
      [ "Failed parsing read-size '",
        val,
        "': ",
        err
      ]

expectedFailure :: (Show a) => Text -> a -> String
expectedFailure val x =
  mconcat
    [ "Expected error parsing read-size '",
      unpack val,
      "', received: ",
      show x
    ]

mkBytesString :: Natural -> Text
mkBytesString x = showt x <> "b"
