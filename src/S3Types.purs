
module AWS.S3.Types where

import Prelude
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Foreign.Generic.Types (Options)
import Data.Foreign.NullOrUndefined (NullOrUndefined(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.StrMap (StrMap) as StrMap

import AWS.Request.Types as Types

options :: Options
options = defaultOptions { unwrapSingleConstructors = true }


newtype AbortDate = AbortDate Types.Timestamp
derive instance newtypeAbortDate :: Newtype AbortDate _
derive instance repGenericAbortDate :: Generic AbortDate _
instance showAbortDate :: Show AbortDate where show = genericShow
instance decodeAbortDate :: Decode AbortDate where decode = genericDecode options
instance encodeAbortDate :: Encode AbortDate where encode = genericEncode options



-- | Specifies the days since the initiation of an Incomplete Multipart Upload that Lifecycle will wait before permanently removing all parts of the upload.
newtype AbortIncompleteMultipartUpload = AbortIncompleteMultipartUpload 
  { "DaysAfterInitiation" :: NullOrUndefined (DaysAfterInitiation)
  }
derive instance newtypeAbortIncompleteMultipartUpload :: Newtype AbortIncompleteMultipartUpload _
derive instance repGenericAbortIncompleteMultipartUpload :: Generic AbortIncompleteMultipartUpload _
instance showAbortIncompleteMultipartUpload :: Show AbortIncompleteMultipartUpload where show = genericShow
instance decodeAbortIncompleteMultipartUpload :: Decode AbortIncompleteMultipartUpload where decode = genericDecode options
instance encodeAbortIncompleteMultipartUpload :: Encode AbortIncompleteMultipartUpload where encode = genericEncode options

-- | Constructs AbortIncompleteMultipartUpload from required parameters
newAbortIncompleteMultipartUpload :: AbortIncompleteMultipartUpload
newAbortIncompleteMultipartUpload  = AbortIncompleteMultipartUpload { "DaysAfterInitiation": (NullOrUndefined Nothing) }

-- | Constructs AbortIncompleteMultipartUpload's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newAbortIncompleteMultipartUpload' :: ( { "DaysAfterInitiation" :: NullOrUndefined (DaysAfterInitiation) } -> {"DaysAfterInitiation" :: NullOrUndefined (DaysAfterInitiation) } ) -> AbortIncompleteMultipartUpload
newAbortIncompleteMultipartUpload'  customize = (AbortIncompleteMultipartUpload <<< customize) { "DaysAfterInitiation": (NullOrUndefined Nothing) }



newtype AbortMultipartUploadOutput = AbortMultipartUploadOutput 
  { "RequestCharged" :: NullOrUndefined (RequestCharged)
  }
derive instance newtypeAbortMultipartUploadOutput :: Newtype AbortMultipartUploadOutput _
derive instance repGenericAbortMultipartUploadOutput :: Generic AbortMultipartUploadOutput _
instance showAbortMultipartUploadOutput :: Show AbortMultipartUploadOutput where show = genericShow
instance decodeAbortMultipartUploadOutput :: Decode AbortMultipartUploadOutput where decode = genericDecode options
instance encodeAbortMultipartUploadOutput :: Encode AbortMultipartUploadOutput where encode = genericEncode options

-- | Constructs AbortMultipartUploadOutput from required parameters
newAbortMultipartUploadOutput :: AbortMultipartUploadOutput
newAbortMultipartUploadOutput  = AbortMultipartUploadOutput { "RequestCharged": (NullOrUndefined Nothing) }

-- | Constructs AbortMultipartUploadOutput's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newAbortMultipartUploadOutput' :: ( { "RequestCharged" :: NullOrUndefined (RequestCharged) } -> {"RequestCharged" :: NullOrUndefined (RequestCharged) } ) -> AbortMultipartUploadOutput
newAbortMultipartUploadOutput'  customize = (AbortMultipartUploadOutput <<< customize) { "RequestCharged": (NullOrUndefined Nothing) }



newtype AbortMultipartUploadRequest = AbortMultipartUploadRequest 
  { "Bucket" :: (BucketName)
  , "Key" :: (ObjectKey)
  , "UploadId" :: (MultipartUploadId)
  , "RequestPayer" :: NullOrUndefined (RequestPayer)
  }
derive instance newtypeAbortMultipartUploadRequest :: Newtype AbortMultipartUploadRequest _
derive instance repGenericAbortMultipartUploadRequest :: Generic AbortMultipartUploadRequest _
instance showAbortMultipartUploadRequest :: Show AbortMultipartUploadRequest where show = genericShow
instance decodeAbortMultipartUploadRequest :: Decode AbortMultipartUploadRequest where decode = genericDecode options
instance encodeAbortMultipartUploadRequest :: Encode AbortMultipartUploadRequest where encode = genericEncode options

-- | Constructs AbortMultipartUploadRequest from required parameters
newAbortMultipartUploadRequest :: BucketName -> ObjectKey -> MultipartUploadId -> AbortMultipartUploadRequest
newAbortMultipartUploadRequest _Bucket _Key _UploadId = AbortMultipartUploadRequest { "Bucket": _Bucket, "Key": _Key, "UploadId": _UploadId, "RequestPayer": (NullOrUndefined Nothing) }

-- | Constructs AbortMultipartUploadRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newAbortMultipartUploadRequest' :: BucketName -> ObjectKey -> MultipartUploadId -> ( { "Bucket" :: (BucketName) , "Key" :: (ObjectKey) , "UploadId" :: (MultipartUploadId) , "RequestPayer" :: NullOrUndefined (RequestPayer) } -> {"Bucket" :: (BucketName) , "Key" :: (ObjectKey) , "UploadId" :: (MultipartUploadId) , "RequestPayer" :: NullOrUndefined (RequestPayer) } ) -> AbortMultipartUploadRequest
newAbortMultipartUploadRequest' _Bucket _Key _UploadId customize = (AbortMultipartUploadRequest <<< customize) { "Bucket": _Bucket, "Key": _Key, "UploadId": _UploadId, "RequestPayer": (NullOrUndefined Nothing) }



newtype AbortRuleId = AbortRuleId String
derive instance newtypeAbortRuleId :: Newtype AbortRuleId _
derive instance repGenericAbortRuleId :: Generic AbortRuleId _
instance showAbortRuleId :: Show AbortRuleId where show = genericShow
instance decodeAbortRuleId :: Decode AbortRuleId where decode = genericDecode options
instance encodeAbortRuleId :: Encode AbortRuleId where encode = genericEncode options



newtype AccelerateConfiguration = AccelerateConfiguration 
  { "Status" :: NullOrUndefined (BucketAccelerateStatus)
  }
derive instance newtypeAccelerateConfiguration :: Newtype AccelerateConfiguration _
derive instance repGenericAccelerateConfiguration :: Generic AccelerateConfiguration _
instance showAccelerateConfiguration :: Show AccelerateConfiguration where show = genericShow
instance decodeAccelerateConfiguration :: Decode AccelerateConfiguration where decode = genericDecode options
instance encodeAccelerateConfiguration :: Encode AccelerateConfiguration where encode = genericEncode options

-- | Constructs AccelerateConfiguration from required parameters
newAccelerateConfiguration :: AccelerateConfiguration
newAccelerateConfiguration  = AccelerateConfiguration { "Status": (NullOrUndefined Nothing) }

-- | Constructs AccelerateConfiguration's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newAccelerateConfiguration' :: ( { "Status" :: NullOrUndefined (BucketAccelerateStatus) } -> {"Status" :: NullOrUndefined (BucketAccelerateStatus) } ) -> AccelerateConfiguration
newAccelerateConfiguration'  customize = (AccelerateConfiguration <<< customize) { "Status": (NullOrUndefined Nothing) }



newtype AcceptRanges = AcceptRanges String
derive instance newtypeAcceptRanges :: Newtype AcceptRanges _
derive instance repGenericAcceptRanges :: Generic AcceptRanges _
instance showAcceptRanges :: Show AcceptRanges where show = genericShow
instance decodeAcceptRanges :: Decode AcceptRanges where decode = genericDecode options
instance encodeAcceptRanges :: Encode AcceptRanges where encode = genericEncode options



newtype AccessControlPolicy = AccessControlPolicy 
  { "Grants" :: NullOrUndefined (Grants)
  , "Owner" :: NullOrUndefined (Owner)
  }
derive instance newtypeAccessControlPolicy :: Newtype AccessControlPolicy _
derive instance repGenericAccessControlPolicy :: Generic AccessControlPolicy _
instance showAccessControlPolicy :: Show AccessControlPolicy where show = genericShow
instance decodeAccessControlPolicy :: Decode AccessControlPolicy where decode = genericDecode options
instance encodeAccessControlPolicy :: Encode AccessControlPolicy where encode = genericEncode options

-- | Constructs AccessControlPolicy from required parameters
newAccessControlPolicy :: AccessControlPolicy
newAccessControlPolicy  = AccessControlPolicy { "Grants": (NullOrUndefined Nothing), "Owner": (NullOrUndefined Nothing) }

-- | Constructs AccessControlPolicy's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newAccessControlPolicy' :: ( { "Grants" :: NullOrUndefined (Grants) , "Owner" :: NullOrUndefined (Owner) } -> {"Grants" :: NullOrUndefined (Grants) , "Owner" :: NullOrUndefined (Owner) } ) -> AccessControlPolicy
newAccessControlPolicy'  customize = (AccessControlPolicy <<< customize) { "Grants": (NullOrUndefined Nothing), "Owner": (NullOrUndefined Nothing) }



-- | Container for information regarding the access control for replicas.
newtype AccessControlTranslation = AccessControlTranslation 
  { "Owner" :: (OwnerOverride)
  }
derive instance newtypeAccessControlTranslation :: Newtype AccessControlTranslation _
derive instance repGenericAccessControlTranslation :: Generic AccessControlTranslation _
instance showAccessControlTranslation :: Show AccessControlTranslation where show = genericShow
instance decodeAccessControlTranslation :: Decode AccessControlTranslation where decode = genericDecode options
instance encodeAccessControlTranslation :: Encode AccessControlTranslation where encode = genericEncode options

-- | Constructs AccessControlTranslation from required parameters
newAccessControlTranslation :: OwnerOverride -> AccessControlTranslation
newAccessControlTranslation _Owner = AccessControlTranslation { "Owner": _Owner }

-- | Constructs AccessControlTranslation's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newAccessControlTranslation' :: OwnerOverride -> ( { "Owner" :: (OwnerOverride) } -> {"Owner" :: (OwnerOverride) } ) -> AccessControlTranslation
newAccessControlTranslation' _Owner customize = (AccessControlTranslation <<< customize) { "Owner": _Owner }



newtype AccountId = AccountId String
derive instance newtypeAccountId :: Newtype AccountId _
derive instance repGenericAccountId :: Generic AccountId _
instance showAccountId :: Show AccountId where show = genericShow
instance decodeAccountId :: Decode AccountId where decode = genericDecode options
instance encodeAccountId :: Encode AccountId where encode = genericEncode options



newtype AllowedHeader = AllowedHeader String
derive instance newtypeAllowedHeader :: Newtype AllowedHeader _
derive instance repGenericAllowedHeader :: Generic AllowedHeader _
instance showAllowedHeader :: Show AllowedHeader where show = genericShow
instance decodeAllowedHeader :: Decode AllowedHeader where decode = genericDecode options
instance encodeAllowedHeader :: Encode AllowedHeader where encode = genericEncode options



newtype AllowedHeaders = AllowedHeaders (Array AllowedHeader)
derive instance newtypeAllowedHeaders :: Newtype AllowedHeaders _
derive instance repGenericAllowedHeaders :: Generic AllowedHeaders _
instance showAllowedHeaders :: Show AllowedHeaders where show = genericShow
instance decodeAllowedHeaders :: Decode AllowedHeaders where decode = genericDecode options
instance encodeAllowedHeaders :: Encode AllowedHeaders where encode = genericEncode options



newtype AllowedMethod = AllowedMethod String
derive instance newtypeAllowedMethod :: Newtype AllowedMethod _
derive instance repGenericAllowedMethod :: Generic AllowedMethod _
instance showAllowedMethod :: Show AllowedMethod where show = genericShow
instance decodeAllowedMethod :: Decode AllowedMethod where decode = genericDecode options
instance encodeAllowedMethod :: Encode AllowedMethod where encode = genericEncode options



newtype AllowedMethods = AllowedMethods (Array AllowedMethod)
derive instance newtypeAllowedMethods :: Newtype AllowedMethods _
derive instance repGenericAllowedMethods :: Generic AllowedMethods _
instance showAllowedMethods :: Show AllowedMethods where show = genericShow
instance decodeAllowedMethods :: Decode AllowedMethods where decode = genericDecode options
instance encodeAllowedMethods :: Encode AllowedMethods where encode = genericEncode options



newtype AllowedOrigin = AllowedOrigin String
derive instance newtypeAllowedOrigin :: Newtype AllowedOrigin _
derive instance repGenericAllowedOrigin :: Generic AllowedOrigin _
instance showAllowedOrigin :: Show AllowedOrigin where show = genericShow
instance decodeAllowedOrigin :: Decode AllowedOrigin where decode = genericDecode options
instance encodeAllowedOrigin :: Encode AllowedOrigin where encode = genericEncode options



newtype AllowedOrigins = AllowedOrigins (Array AllowedOrigin)
derive instance newtypeAllowedOrigins :: Newtype AllowedOrigins _
derive instance repGenericAllowedOrigins :: Generic AllowedOrigins _
instance showAllowedOrigins :: Show AllowedOrigins where show = genericShow
instance decodeAllowedOrigins :: Decode AllowedOrigins where decode = genericDecode options
instance encodeAllowedOrigins :: Encode AllowedOrigins where encode = genericEncode options



newtype AnalyticsAndOperator = AnalyticsAndOperator 
  { "Prefix" :: NullOrUndefined (Prefix)
  , "Tags" :: NullOrUndefined (TagSet)
  }
derive instance newtypeAnalyticsAndOperator :: Newtype AnalyticsAndOperator _
derive instance repGenericAnalyticsAndOperator :: Generic AnalyticsAndOperator _
instance showAnalyticsAndOperator :: Show AnalyticsAndOperator where show = genericShow
instance decodeAnalyticsAndOperator :: Decode AnalyticsAndOperator where decode = genericDecode options
instance encodeAnalyticsAndOperator :: Encode AnalyticsAndOperator where encode = genericEncode options

-- | Constructs AnalyticsAndOperator from required parameters
newAnalyticsAndOperator :: AnalyticsAndOperator
newAnalyticsAndOperator  = AnalyticsAndOperator { "Prefix": (NullOrUndefined Nothing), "Tags": (NullOrUndefined Nothing) }

-- | Constructs AnalyticsAndOperator's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newAnalyticsAndOperator' :: ( { "Prefix" :: NullOrUndefined (Prefix) , "Tags" :: NullOrUndefined (TagSet) } -> {"Prefix" :: NullOrUndefined (Prefix) , "Tags" :: NullOrUndefined (TagSet) } ) -> AnalyticsAndOperator
newAnalyticsAndOperator'  customize = (AnalyticsAndOperator <<< customize) { "Prefix": (NullOrUndefined Nothing), "Tags": (NullOrUndefined Nothing) }



newtype AnalyticsConfiguration = AnalyticsConfiguration 
  { "Id" :: (AnalyticsId)
  , "Filter" :: NullOrUndefined (AnalyticsFilter)
  , "StorageClassAnalysis" :: (StorageClassAnalysis)
  }
derive instance newtypeAnalyticsConfiguration :: Newtype AnalyticsConfiguration _
derive instance repGenericAnalyticsConfiguration :: Generic AnalyticsConfiguration _
instance showAnalyticsConfiguration :: Show AnalyticsConfiguration where show = genericShow
instance decodeAnalyticsConfiguration :: Decode AnalyticsConfiguration where decode = genericDecode options
instance encodeAnalyticsConfiguration :: Encode AnalyticsConfiguration where encode = genericEncode options

-- | Constructs AnalyticsConfiguration from required parameters
newAnalyticsConfiguration :: AnalyticsId -> StorageClassAnalysis -> AnalyticsConfiguration
newAnalyticsConfiguration _Id _StorageClassAnalysis = AnalyticsConfiguration { "Id": _Id, "StorageClassAnalysis": _StorageClassAnalysis, "Filter": (NullOrUndefined Nothing) }

-- | Constructs AnalyticsConfiguration's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newAnalyticsConfiguration' :: AnalyticsId -> StorageClassAnalysis -> ( { "Id" :: (AnalyticsId) , "Filter" :: NullOrUndefined (AnalyticsFilter) , "StorageClassAnalysis" :: (StorageClassAnalysis) } -> {"Id" :: (AnalyticsId) , "Filter" :: NullOrUndefined (AnalyticsFilter) , "StorageClassAnalysis" :: (StorageClassAnalysis) } ) -> AnalyticsConfiguration
newAnalyticsConfiguration' _Id _StorageClassAnalysis customize = (AnalyticsConfiguration <<< customize) { "Id": _Id, "StorageClassAnalysis": _StorageClassAnalysis, "Filter": (NullOrUndefined Nothing) }



newtype AnalyticsConfigurationList = AnalyticsConfigurationList (Array AnalyticsConfiguration)
derive instance newtypeAnalyticsConfigurationList :: Newtype AnalyticsConfigurationList _
derive instance repGenericAnalyticsConfigurationList :: Generic AnalyticsConfigurationList _
instance showAnalyticsConfigurationList :: Show AnalyticsConfigurationList where show = genericShow
instance decodeAnalyticsConfigurationList :: Decode AnalyticsConfigurationList where decode = genericDecode options
instance encodeAnalyticsConfigurationList :: Encode AnalyticsConfigurationList where encode = genericEncode options



newtype AnalyticsExportDestination = AnalyticsExportDestination 
  { "S3BucketDestination" :: (AnalyticsS3BucketDestination)
  }
derive instance newtypeAnalyticsExportDestination :: Newtype AnalyticsExportDestination _
derive instance repGenericAnalyticsExportDestination :: Generic AnalyticsExportDestination _
instance showAnalyticsExportDestination :: Show AnalyticsExportDestination where show = genericShow
instance decodeAnalyticsExportDestination :: Decode AnalyticsExportDestination where decode = genericDecode options
instance encodeAnalyticsExportDestination :: Encode AnalyticsExportDestination where encode = genericEncode options

-- | Constructs AnalyticsExportDestination from required parameters
newAnalyticsExportDestination :: AnalyticsS3BucketDestination -> AnalyticsExportDestination
newAnalyticsExportDestination _S3BucketDestination = AnalyticsExportDestination { "S3BucketDestination": _S3BucketDestination }

-- | Constructs AnalyticsExportDestination's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newAnalyticsExportDestination' :: AnalyticsS3BucketDestination -> ( { "S3BucketDestination" :: (AnalyticsS3BucketDestination) } -> {"S3BucketDestination" :: (AnalyticsS3BucketDestination) } ) -> AnalyticsExportDestination
newAnalyticsExportDestination' _S3BucketDestination customize = (AnalyticsExportDestination <<< customize) { "S3BucketDestination": _S3BucketDestination }



newtype AnalyticsFilter = AnalyticsFilter 
  { "Prefix" :: NullOrUndefined (Prefix)
  , "Tag" :: NullOrUndefined (Tag)
  , "And" :: NullOrUndefined (AnalyticsAndOperator)
  }
derive instance newtypeAnalyticsFilter :: Newtype AnalyticsFilter _
derive instance repGenericAnalyticsFilter :: Generic AnalyticsFilter _
instance showAnalyticsFilter :: Show AnalyticsFilter where show = genericShow
instance decodeAnalyticsFilter :: Decode AnalyticsFilter where decode = genericDecode options
instance encodeAnalyticsFilter :: Encode AnalyticsFilter where encode = genericEncode options

-- | Constructs AnalyticsFilter from required parameters
newAnalyticsFilter :: AnalyticsFilter
newAnalyticsFilter  = AnalyticsFilter { "And": (NullOrUndefined Nothing), "Prefix": (NullOrUndefined Nothing), "Tag": (NullOrUndefined Nothing) }

-- | Constructs AnalyticsFilter's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newAnalyticsFilter' :: ( { "Prefix" :: NullOrUndefined (Prefix) , "Tag" :: NullOrUndefined (Tag) , "And" :: NullOrUndefined (AnalyticsAndOperator) } -> {"Prefix" :: NullOrUndefined (Prefix) , "Tag" :: NullOrUndefined (Tag) , "And" :: NullOrUndefined (AnalyticsAndOperator) } ) -> AnalyticsFilter
newAnalyticsFilter'  customize = (AnalyticsFilter <<< customize) { "And": (NullOrUndefined Nothing), "Prefix": (NullOrUndefined Nothing), "Tag": (NullOrUndefined Nothing) }



newtype AnalyticsId = AnalyticsId String
derive instance newtypeAnalyticsId :: Newtype AnalyticsId _
derive instance repGenericAnalyticsId :: Generic AnalyticsId _
instance showAnalyticsId :: Show AnalyticsId where show = genericShow
instance decodeAnalyticsId :: Decode AnalyticsId where decode = genericDecode options
instance encodeAnalyticsId :: Encode AnalyticsId where encode = genericEncode options



newtype AnalyticsS3BucketDestination = AnalyticsS3BucketDestination 
  { "Format" :: (AnalyticsS3ExportFileFormat)
  , "BucketAccountId" :: NullOrUndefined (AccountId)
  , "Bucket" :: (BucketName)
  , "Prefix" :: NullOrUndefined (Prefix)
  }
derive instance newtypeAnalyticsS3BucketDestination :: Newtype AnalyticsS3BucketDestination _
derive instance repGenericAnalyticsS3BucketDestination :: Generic AnalyticsS3BucketDestination _
instance showAnalyticsS3BucketDestination :: Show AnalyticsS3BucketDestination where show = genericShow
instance decodeAnalyticsS3BucketDestination :: Decode AnalyticsS3BucketDestination where decode = genericDecode options
instance encodeAnalyticsS3BucketDestination :: Encode AnalyticsS3BucketDestination where encode = genericEncode options

-- | Constructs AnalyticsS3BucketDestination from required parameters
newAnalyticsS3BucketDestination :: BucketName -> AnalyticsS3ExportFileFormat -> AnalyticsS3BucketDestination
newAnalyticsS3BucketDestination _Bucket _Format = AnalyticsS3BucketDestination { "Bucket": _Bucket, "Format": _Format, "BucketAccountId": (NullOrUndefined Nothing), "Prefix": (NullOrUndefined Nothing) }

-- | Constructs AnalyticsS3BucketDestination's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newAnalyticsS3BucketDestination' :: BucketName -> AnalyticsS3ExportFileFormat -> ( { "Format" :: (AnalyticsS3ExportFileFormat) , "BucketAccountId" :: NullOrUndefined (AccountId) , "Bucket" :: (BucketName) , "Prefix" :: NullOrUndefined (Prefix) } -> {"Format" :: (AnalyticsS3ExportFileFormat) , "BucketAccountId" :: NullOrUndefined (AccountId) , "Bucket" :: (BucketName) , "Prefix" :: NullOrUndefined (Prefix) } ) -> AnalyticsS3BucketDestination
newAnalyticsS3BucketDestination' _Bucket _Format customize = (AnalyticsS3BucketDestination <<< customize) { "Bucket": _Bucket, "Format": _Format, "BucketAccountId": (NullOrUndefined Nothing), "Prefix": (NullOrUndefined Nothing) }



newtype AnalyticsS3ExportFileFormat = AnalyticsS3ExportFileFormat String
derive instance newtypeAnalyticsS3ExportFileFormat :: Newtype AnalyticsS3ExportFileFormat _
derive instance repGenericAnalyticsS3ExportFileFormat :: Generic AnalyticsS3ExportFileFormat _
instance showAnalyticsS3ExportFileFormat :: Show AnalyticsS3ExportFileFormat where show = genericShow
instance decodeAnalyticsS3ExportFileFormat :: Decode AnalyticsS3ExportFileFormat where decode = genericDecode options
instance encodeAnalyticsS3ExportFileFormat :: Encode AnalyticsS3ExportFileFormat where encode = genericEncode options



newtype Body = Body String
derive instance newtypeBody :: Newtype Body _
derive instance repGenericBody :: Generic Body _
instance showBody :: Show Body where show = genericShow
instance decodeBody :: Decode Body where decode = genericDecode options
instance encodeBody :: Encode Body where encode = genericEncode options



newtype Bucket = Bucket 
  { "Name" :: NullOrUndefined (BucketName)
  , "CreationDate" :: NullOrUndefined (CreationDate)
  }
derive instance newtypeBucket :: Newtype Bucket _
derive instance repGenericBucket :: Generic Bucket _
instance showBucket :: Show Bucket where show = genericShow
instance decodeBucket :: Decode Bucket where decode = genericDecode options
instance encodeBucket :: Encode Bucket where encode = genericEncode options

-- | Constructs Bucket from required parameters
newBucket :: Bucket
newBucket  = Bucket { "CreationDate": (NullOrUndefined Nothing), "Name": (NullOrUndefined Nothing) }

-- | Constructs Bucket's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newBucket' :: ( { "Name" :: NullOrUndefined (BucketName) , "CreationDate" :: NullOrUndefined (CreationDate) } -> {"Name" :: NullOrUndefined (BucketName) , "CreationDate" :: NullOrUndefined (CreationDate) } ) -> Bucket
newBucket'  customize = (Bucket <<< customize) { "CreationDate": (NullOrUndefined Nothing), "Name": (NullOrUndefined Nothing) }



newtype BucketAccelerateStatus = BucketAccelerateStatus String
derive instance newtypeBucketAccelerateStatus :: Newtype BucketAccelerateStatus _
derive instance repGenericBucketAccelerateStatus :: Generic BucketAccelerateStatus _
instance showBucketAccelerateStatus :: Show BucketAccelerateStatus where show = genericShow
instance decodeBucketAccelerateStatus :: Decode BucketAccelerateStatus where decode = genericDecode options
instance encodeBucketAccelerateStatus :: Encode BucketAccelerateStatus where encode = genericEncode options



-- | The requested bucket name is not available. The bucket namespace is shared by all users of the system. Please select a different name and try again.
newtype BucketAlreadyExists = BucketAlreadyExists Types.NoArguments
derive instance newtypeBucketAlreadyExists :: Newtype BucketAlreadyExists _
derive instance repGenericBucketAlreadyExists :: Generic BucketAlreadyExists _
instance showBucketAlreadyExists :: Show BucketAlreadyExists where show = genericShow
instance decodeBucketAlreadyExists :: Decode BucketAlreadyExists where decode = genericDecode options
instance encodeBucketAlreadyExists :: Encode BucketAlreadyExists where encode = genericEncode options



newtype BucketAlreadyOwnedByYou = BucketAlreadyOwnedByYou Types.NoArguments
derive instance newtypeBucketAlreadyOwnedByYou :: Newtype BucketAlreadyOwnedByYou _
derive instance repGenericBucketAlreadyOwnedByYou :: Generic BucketAlreadyOwnedByYou _
instance showBucketAlreadyOwnedByYou :: Show BucketAlreadyOwnedByYou where show = genericShow
instance decodeBucketAlreadyOwnedByYou :: Decode BucketAlreadyOwnedByYou where decode = genericDecode options
instance encodeBucketAlreadyOwnedByYou :: Encode BucketAlreadyOwnedByYou where encode = genericEncode options



newtype BucketCannedACL = BucketCannedACL String
derive instance newtypeBucketCannedACL :: Newtype BucketCannedACL _
derive instance repGenericBucketCannedACL :: Generic BucketCannedACL _
instance showBucketCannedACL :: Show BucketCannedACL where show = genericShow
instance decodeBucketCannedACL :: Decode BucketCannedACL where decode = genericDecode options
instance encodeBucketCannedACL :: Encode BucketCannedACL where encode = genericEncode options



newtype BucketLifecycleConfiguration = BucketLifecycleConfiguration 
  { "Rules" :: (LifecycleRules)
  }
derive instance newtypeBucketLifecycleConfiguration :: Newtype BucketLifecycleConfiguration _
derive instance repGenericBucketLifecycleConfiguration :: Generic BucketLifecycleConfiguration _
instance showBucketLifecycleConfiguration :: Show BucketLifecycleConfiguration where show = genericShow
instance decodeBucketLifecycleConfiguration :: Decode BucketLifecycleConfiguration where decode = genericDecode options
instance encodeBucketLifecycleConfiguration :: Encode BucketLifecycleConfiguration where encode = genericEncode options

-- | Constructs BucketLifecycleConfiguration from required parameters
newBucketLifecycleConfiguration :: LifecycleRules -> BucketLifecycleConfiguration
newBucketLifecycleConfiguration _Rules = BucketLifecycleConfiguration { "Rules": _Rules }

-- | Constructs BucketLifecycleConfiguration's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newBucketLifecycleConfiguration' :: LifecycleRules -> ( { "Rules" :: (LifecycleRules) } -> {"Rules" :: (LifecycleRules) } ) -> BucketLifecycleConfiguration
newBucketLifecycleConfiguration' _Rules customize = (BucketLifecycleConfiguration <<< customize) { "Rules": _Rules }



newtype BucketLocationConstraint = BucketLocationConstraint String
derive instance newtypeBucketLocationConstraint :: Newtype BucketLocationConstraint _
derive instance repGenericBucketLocationConstraint :: Generic BucketLocationConstraint _
instance showBucketLocationConstraint :: Show BucketLocationConstraint where show = genericShow
instance decodeBucketLocationConstraint :: Decode BucketLocationConstraint where decode = genericDecode options
instance encodeBucketLocationConstraint :: Encode BucketLocationConstraint where encode = genericEncode options



newtype BucketLoggingStatus = BucketLoggingStatus 
  { "LoggingEnabled" :: NullOrUndefined (LoggingEnabled)
  }
derive instance newtypeBucketLoggingStatus :: Newtype BucketLoggingStatus _
derive instance repGenericBucketLoggingStatus :: Generic BucketLoggingStatus _
instance showBucketLoggingStatus :: Show BucketLoggingStatus where show = genericShow
instance decodeBucketLoggingStatus :: Decode BucketLoggingStatus where decode = genericDecode options
instance encodeBucketLoggingStatus :: Encode BucketLoggingStatus where encode = genericEncode options

-- | Constructs BucketLoggingStatus from required parameters
newBucketLoggingStatus :: BucketLoggingStatus
newBucketLoggingStatus  = BucketLoggingStatus { "LoggingEnabled": (NullOrUndefined Nothing) }

-- | Constructs BucketLoggingStatus's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newBucketLoggingStatus' :: ( { "LoggingEnabled" :: NullOrUndefined (LoggingEnabled) } -> {"LoggingEnabled" :: NullOrUndefined (LoggingEnabled) } ) -> BucketLoggingStatus
newBucketLoggingStatus'  customize = (BucketLoggingStatus <<< customize) { "LoggingEnabled": (NullOrUndefined Nothing) }



newtype BucketLogsPermission = BucketLogsPermission String
derive instance newtypeBucketLogsPermission :: Newtype BucketLogsPermission _
derive instance repGenericBucketLogsPermission :: Generic BucketLogsPermission _
instance showBucketLogsPermission :: Show BucketLogsPermission where show = genericShow
instance decodeBucketLogsPermission :: Decode BucketLogsPermission where decode = genericDecode options
instance encodeBucketLogsPermission :: Encode BucketLogsPermission where encode = genericEncode options



newtype BucketName = BucketName String
derive instance newtypeBucketName :: Newtype BucketName _
derive instance repGenericBucketName :: Generic BucketName _
instance showBucketName :: Show BucketName where show = genericShow
instance decodeBucketName :: Decode BucketName where decode = genericDecode options
instance encodeBucketName :: Encode BucketName where encode = genericEncode options



newtype BucketVersioningStatus = BucketVersioningStatus String
derive instance newtypeBucketVersioningStatus :: Newtype BucketVersioningStatus _
derive instance repGenericBucketVersioningStatus :: Generic BucketVersioningStatus _
instance showBucketVersioningStatus :: Show BucketVersioningStatus where show = genericShow
instance decodeBucketVersioningStatus :: Decode BucketVersioningStatus where decode = genericDecode options
instance encodeBucketVersioningStatus :: Encode BucketVersioningStatus where encode = genericEncode options



newtype Buckets = Buckets (Array Bucket)
derive instance newtypeBuckets :: Newtype Buckets _
derive instance repGenericBuckets :: Generic Buckets _
instance showBuckets :: Show Buckets where show = genericShow
instance decodeBuckets :: Decode Buckets where decode = genericDecode options
instance encodeBuckets :: Encode Buckets where encode = genericEncode options



newtype CORSConfiguration = CORSConfiguration 
  { "CORSRules" :: (CORSRules)
  }
derive instance newtypeCORSConfiguration :: Newtype CORSConfiguration _
derive instance repGenericCORSConfiguration :: Generic CORSConfiguration _
instance showCORSConfiguration :: Show CORSConfiguration where show = genericShow
instance decodeCORSConfiguration :: Decode CORSConfiguration where decode = genericDecode options
instance encodeCORSConfiguration :: Encode CORSConfiguration where encode = genericEncode options

-- | Constructs CORSConfiguration from required parameters
newCORSConfiguration :: CORSRules -> CORSConfiguration
newCORSConfiguration _CORSRules = CORSConfiguration { "CORSRules": _CORSRules }

-- | Constructs CORSConfiguration's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newCORSConfiguration' :: CORSRules -> ( { "CORSRules" :: (CORSRules) } -> {"CORSRules" :: (CORSRules) } ) -> CORSConfiguration
newCORSConfiguration' _CORSRules customize = (CORSConfiguration <<< customize) { "CORSRules": _CORSRules }



newtype CORSRule = CORSRule 
  { "AllowedHeaders" :: NullOrUndefined (AllowedHeaders)
  , "AllowedMethods" :: (AllowedMethods)
  , "AllowedOrigins" :: (AllowedOrigins)
  , "ExposeHeaders" :: NullOrUndefined (ExposeHeaders)
  , "MaxAgeSeconds" :: NullOrUndefined (MaxAgeSeconds)
  }
derive instance newtypeCORSRule :: Newtype CORSRule _
derive instance repGenericCORSRule :: Generic CORSRule _
instance showCORSRule :: Show CORSRule where show = genericShow
instance decodeCORSRule :: Decode CORSRule where decode = genericDecode options
instance encodeCORSRule :: Encode CORSRule where encode = genericEncode options

-- | Constructs CORSRule from required parameters
newCORSRule :: AllowedMethods -> AllowedOrigins -> CORSRule
newCORSRule _AllowedMethods _AllowedOrigins = CORSRule { "AllowedMethods": _AllowedMethods, "AllowedOrigins": _AllowedOrigins, "AllowedHeaders": (NullOrUndefined Nothing), "ExposeHeaders": (NullOrUndefined Nothing), "MaxAgeSeconds": (NullOrUndefined Nothing) }

-- | Constructs CORSRule's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newCORSRule' :: AllowedMethods -> AllowedOrigins -> ( { "AllowedHeaders" :: NullOrUndefined (AllowedHeaders) , "AllowedMethods" :: (AllowedMethods) , "AllowedOrigins" :: (AllowedOrigins) , "ExposeHeaders" :: NullOrUndefined (ExposeHeaders) , "MaxAgeSeconds" :: NullOrUndefined (MaxAgeSeconds) } -> {"AllowedHeaders" :: NullOrUndefined (AllowedHeaders) , "AllowedMethods" :: (AllowedMethods) , "AllowedOrigins" :: (AllowedOrigins) , "ExposeHeaders" :: NullOrUndefined (ExposeHeaders) , "MaxAgeSeconds" :: NullOrUndefined (MaxAgeSeconds) } ) -> CORSRule
newCORSRule' _AllowedMethods _AllowedOrigins customize = (CORSRule <<< customize) { "AllowedMethods": _AllowedMethods, "AllowedOrigins": _AllowedOrigins, "AllowedHeaders": (NullOrUndefined Nothing), "ExposeHeaders": (NullOrUndefined Nothing), "MaxAgeSeconds": (NullOrUndefined Nothing) }



newtype CORSRules = CORSRules (Array CORSRule)
derive instance newtypeCORSRules :: Newtype CORSRules _
derive instance repGenericCORSRules :: Generic CORSRules _
instance showCORSRules :: Show CORSRules where show = genericShow
instance decodeCORSRules :: Decode CORSRules where decode = genericDecode options
instance encodeCORSRules :: Encode CORSRules where encode = genericEncode options



-- | Describes how a CSV-formatted input object is formatted.
newtype CSVInput = CSVInput 
  { "FileHeaderInfo" :: NullOrUndefined (FileHeaderInfo)
  , "Comments" :: NullOrUndefined (Comments)
  , "QuoteEscapeCharacter" :: NullOrUndefined (QuoteEscapeCharacter)
  , "RecordDelimiter" :: NullOrUndefined (RecordDelimiter)
  , "FieldDelimiter" :: NullOrUndefined (FieldDelimiter)
  , "QuoteCharacter" :: NullOrUndefined (QuoteCharacter)
  }
derive instance newtypeCSVInput :: Newtype CSVInput _
derive instance repGenericCSVInput :: Generic CSVInput _
instance showCSVInput :: Show CSVInput where show = genericShow
instance decodeCSVInput :: Decode CSVInput where decode = genericDecode options
instance encodeCSVInput :: Encode CSVInput where encode = genericEncode options

-- | Constructs CSVInput from required parameters
newCSVInput :: CSVInput
newCSVInput  = CSVInput { "Comments": (NullOrUndefined Nothing), "FieldDelimiter": (NullOrUndefined Nothing), "FileHeaderInfo": (NullOrUndefined Nothing), "QuoteCharacter": (NullOrUndefined Nothing), "QuoteEscapeCharacter": (NullOrUndefined Nothing), "RecordDelimiter": (NullOrUndefined Nothing) }

-- | Constructs CSVInput's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newCSVInput' :: ( { "FileHeaderInfo" :: NullOrUndefined (FileHeaderInfo) , "Comments" :: NullOrUndefined (Comments) , "QuoteEscapeCharacter" :: NullOrUndefined (QuoteEscapeCharacter) , "RecordDelimiter" :: NullOrUndefined (RecordDelimiter) , "FieldDelimiter" :: NullOrUndefined (FieldDelimiter) , "QuoteCharacter" :: NullOrUndefined (QuoteCharacter) } -> {"FileHeaderInfo" :: NullOrUndefined (FileHeaderInfo) , "Comments" :: NullOrUndefined (Comments) , "QuoteEscapeCharacter" :: NullOrUndefined (QuoteEscapeCharacter) , "RecordDelimiter" :: NullOrUndefined (RecordDelimiter) , "FieldDelimiter" :: NullOrUndefined (FieldDelimiter) , "QuoteCharacter" :: NullOrUndefined (QuoteCharacter) } ) -> CSVInput
newCSVInput'  customize = (CSVInput <<< customize) { "Comments": (NullOrUndefined Nothing), "FieldDelimiter": (NullOrUndefined Nothing), "FileHeaderInfo": (NullOrUndefined Nothing), "QuoteCharacter": (NullOrUndefined Nothing), "QuoteEscapeCharacter": (NullOrUndefined Nothing), "RecordDelimiter": (NullOrUndefined Nothing) }



-- | Describes how CSV-formatted results are formatted.
newtype CSVOutput = CSVOutput 
  { "QuoteFields" :: NullOrUndefined (QuoteFields)
  , "QuoteEscapeCharacter" :: NullOrUndefined (QuoteEscapeCharacter)
  , "RecordDelimiter" :: NullOrUndefined (RecordDelimiter)
  , "FieldDelimiter" :: NullOrUndefined (FieldDelimiter)
  , "QuoteCharacter" :: NullOrUndefined (QuoteCharacter)
  }
derive instance newtypeCSVOutput :: Newtype CSVOutput _
derive instance repGenericCSVOutput :: Generic CSVOutput _
instance showCSVOutput :: Show CSVOutput where show = genericShow
instance decodeCSVOutput :: Decode CSVOutput where decode = genericDecode options
instance encodeCSVOutput :: Encode CSVOutput where encode = genericEncode options

-- | Constructs CSVOutput from required parameters
newCSVOutput :: CSVOutput
newCSVOutput  = CSVOutput { "FieldDelimiter": (NullOrUndefined Nothing), "QuoteCharacter": (NullOrUndefined Nothing), "QuoteEscapeCharacter": (NullOrUndefined Nothing), "QuoteFields": (NullOrUndefined Nothing), "RecordDelimiter": (NullOrUndefined Nothing) }

-- | Constructs CSVOutput's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newCSVOutput' :: ( { "QuoteFields" :: NullOrUndefined (QuoteFields) , "QuoteEscapeCharacter" :: NullOrUndefined (QuoteEscapeCharacter) , "RecordDelimiter" :: NullOrUndefined (RecordDelimiter) , "FieldDelimiter" :: NullOrUndefined (FieldDelimiter) , "QuoteCharacter" :: NullOrUndefined (QuoteCharacter) } -> {"QuoteFields" :: NullOrUndefined (QuoteFields) , "QuoteEscapeCharacter" :: NullOrUndefined (QuoteEscapeCharacter) , "RecordDelimiter" :: NullOrUndefined (RecordDelimiter) , "FieldDelimiter" :: NullOrUndefined (FieldDelimiter) , "QuoteCharacter" :: NullOrUndefined (QuoteCharacter) } ) -> CSVOutput
newCSVOutput'  customize = (CSVOutput <<< customize) { "FieldDelimiter": (NullOrUndefined Nothing), "QuoteCharacter": (NullOrUndefined Nothing), "QuoteEscapeCharacter": (NullOrUndefined Nothing), "QuoteFields": (NullOrUndefined Nothing), "RecordDelimiter": (NullOrUndefined Nothing) }



newtype CacheControl = CacheControl String
derive instance newtypeCacheControl :: Newtype CacheControl _
derive instance repGenericCacheControl :: Generic CacheControl _
instance showCacheControl :: Show CacheControl where show = genericShow
instance decodeCacheControl :: Decode CacheControl where decode = genericDecode options
instance encodeCacheControl :: Encode CacheControl where encode = genericEncode options



newtype CloudFunction = CloudFunction String
derive instance newtypeCloudFunction :: Newtype CloudFunction _
derive instance repGenericCloudFunction :: Generic CloudFunction _
instance showCloudFunction :: Show CloudFunction where show = genericShow
instance decodeCloudFunction :: Decode CloudFunction where decode = genericDecode options
instance encodeCloudFunction :: Encode CloudFunction where encode = genericEncode options



newtype CloudFunctionConfiguration = CloudFunctionConfiguration 
  { "Id" :: NullOrUndefined (NotificationId)
  , "Event" :: NullOrUndefined (Event)
  , "Events" :: NullOrUndefined (EventList)
  , "CloudFunction" :: NullOrUndefined (CloudFunction)
  , "InvocationRole" :: NullOrUndefined (CloudFunctionInvocationRole)
  }
derive instance newtypeCloudFunctionConfiguration :: Newtype CloudFunctionConfiguration _
derive instance repGenericCloudFunctionConfiguration :: Generic CloudFunctionConfiguration _
instance showCloudFunctionConfiguration :: Show CloudFunctionConfiguration where show = genericShow
instance decodeCloudFunctionConfiguration :: Decode CloudFunctionConfiguration where decode = genericDecode options
instance encodeCloudFunctionConfiguration :: Encode CloudFunctionConfiguration where encode = genericEncode options

-- | Constructs CloudFunctionConfiguration from required parameters
newCloudFunctionConfiguration :: CloudFunctionConfiguration
newCloudFunctionConfiguration  = CloudFunctionConfiguration { "CloudFunction": (NullOrUndefined Nothing), "Event": (NullOrUndefined Nothing), "Events": (NullOrUndefined Nothing), "Id": (NullOrUndefined Nothing), "InvocationRole": (NullOrUndefined Nothing) }

-- | Constructs CloudFunctionConfiguration's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newCloudFunctionConfiguration' :: ( { "Id" :: NullOrUndefined (NotificationId) , "Event" :: NullOrUndefined (Event) , "Events" :: NullOrUndefined (EventList) , "CloudFunction" :: NullOrUndefined (CloudFunction) , "InvocationRole" :: NullOrUndefined (CloudFunctionInvocationRole) } -> {"Id" :: NullOrUndefined (NotificationId) , "Event" :: NullOrUndefined (Event) , "Events" :: NullOrUndefined (EventList) , "CloudFunction" :: NullOrUndefined (CloudFunction) , "InvocationRole" :: NullOrUndefined (CloudFunctionInvocationRole) } ) -> CloudFunctionConfiguration
newCloudFunctionConfiguration'  customize = (CloudFunctionConfiguration <<< customize) { "CloudFunction": (NullOrUndefined Nothing), "Event": (NullOrUndefined Nothing), "Events": (NullOrUndefined Nothing), "Id": (NullOrUndefined Nothing), "InvocationRole": (NullOrUndefined Nothing) }



newtype CloudFunctionInvocationRole = CloudFunctionInvocationRole String
derive instance newtypeCloudFunctionInvocationRole :: Newtype CloudFunctionInvocationRole _
derive instance repGenericCloudFunctionInvocationRole :: Generic CloudFunctionInvocationRole _
instance showCloudFunctionInvocationRole :: Show CloudFunctionInvocationRole where show = genericShow
instance decodeCloudFunctionInvocationRole :: Decode CloudFunctionInvocationRole where decode = genericDecode options
instance encodeCloudFunctionInvocationRole :: Encode CloudFunctionInvocationRole where encode = genericEncode options



newtype Code = Code String
derive instance newtypeCode :: Newtype Code _
derive instance repGenericCode :: Generic Code _
instance showCode :: Show Code where show = genericShow
instance decodeCode :: Decode Code where decode = genericDecode options
instance encodeCode :: Encode Code where encode = genericEncode options



newtype Comments = Comments String
derive instance newtypeComments :: Newtype Comments _
derive instance repGenericComments :: Generic Comments _
instance showComments :: Show Comments where show = genericShow
instance decodeComments :: Decode Comments where decode = genericDecode options
instance encodeComments :: Encode Comments where encode = genericEncode options



newtype CommonPrefix = CommonPrefix 
  { "Prefix" :: NullOrUndefined (Prefix)
  }
derive instance newtypeCommonPrefix :: Newtype CommonPrefix _
derive instance repGenericCommonPrefix :: Generic CommonPrefix _
instance showCommonPrefix :: Show CommonPrefix where show = genericShow
instance decodeCommonPrefix :: Decode CommonPrefix where decode = genericDecode options
instance encodeCommonPrefix :: Encode CommonPrefix where encode = genericEncode options

-- | Constructs CommonPrefix from required parameters
newCommonPrefix :: CommonPrefix
newCommonPrefix  = CommonPrefix { "Prefix": (NullOrUndefined Nothing) }

-- | Constructs CommonPrefix's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newCommonPrefix' :: ( { "Prefix" :: NullOrUndefined (Prefix) } -> {"Prefix" :: NullOrUndefined (Prefix) } ) -> CommonPrefix
newCommonPrefix'  customize = (CommonPrefix <<< customize) { "Prefix": (NullOrUndefined Nothing) }



newtype CommonPrefixList = CommonPrefixList (Array CommonPrefix)
derive instance newtypeCommonPrefixList :: Newtype CommonPrefixList _
derive instance repGenericCommonPrefixList :: Generic CommonPrefixList _
instance showCommonPrefixList :: Show CommonPrefixList where show = genericShow
instance decodeCommonPrefixList :: Decode CommonPrefixList where decode = genericDecode options
instance encodeCommonPrefixList :: Encode CommonPrefixList where encode = genericEncode options



newtype CompleteMultipartUploadOutput = CompleteMultipartUploadOutput 
  { "Location" :: NullOrUndefined (Location)
  , "Bucket" :: NullOrUndefined (BucketName)
  , "Key" :: NullOrUndefined (ObjectKey)
  , "Expiration" :: NullOrUndefined (Expiration)
  , "ETag" :: NullOrUndefined (ETag)
  , "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption)
  , "VersionId" :: NullOrUndefined (ObjectVersionId)
  , "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId)
  , "RequestCharged" :: NullOrUndefined (RequestCharged)
  }
derive instance newtypeCompleteMultipartUploadOutput :: Newtype CompleteMultipartUploadOutput _
derive instance repGenericCompleteMultipartUploadOutput :: Generic CompleteMultipartUploadOutput _
instance showCompleteMultipartUploadOutput :: Show CompleteMultipartUploadOutput where show = genericShow
instance decodeCompleteMultipartUploadOutput :: Decode CompleteMultipartUploadOutput where decode = genericDecode options
instance encodeCompleteMultipartUploadOutput :: Encode CompleteMultipartUploadOutput where encode = genericEncode options

-- | Constructs CompleteMultipartUploadOutput from required parameters
newCompleteMultipartUploadOutput :: CompleteMultipartUploadOutput
newCompleteMultipartUploadOutput  = CompleteMultipartUploadOutput { "Bucket": (NullOrUndefined Nothing), "ETag": (NullOrUndefined Nothing), "Expiration": (NullOrUndefined Nothing), "Key": (NullOrUndefined Nothing), "Location": (NullOrUndefined Nothing), "RequestCharged": (NullOrUndefined Nothing), "SSEKMSKeyId": (NullOrUndefined Nothing), "ServerSideEncryption": (NullOrUndefined Nothing), "VersionId": (NullOrUndefined Nothing) }

-- | Constructs CompleteMultipartUploadOutput's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newCompleteMultipartUploadOutput' :: ( { "Location" :: NullOrUndefined (Location) , "Bucket" :: NullOrUndefined (BucketName) , "Key" :: NullOrUndefined (ObjectKey) , "Expiration" :: NullOrUndefined (Expiration) , "ETag" :: NullOrUndefined (ETag) , "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption) , "VersionId" :: NullOrUndefined (ObjectVersionId) , "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId) , "RequestCharged" :: NullOrUndefined (RequestCharged) } -> {"Location" :: NullOrUndefined (Location) , "Bucket" :: NullOrUndefined (BucketName) , "Key" :: NullOrUndefined (ObjectKey) , "Expiration" :: NullOrUndefined (Expiration) , "ETag" :: NullOrUndefined (ETag) , "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption) , "VersionId" :: NullOrUndefined (ObjectVersionId) , "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId) , "RequestCharged" :: NullOrUndefined (RequestCharged) } ) -> CompleteMultipartUploadOutput
newCompleteMultipartUploadOutput'  customize = (CompleteMultipartUploadOutput <<< customize) { "Bucket": (NullOrUndefined Nothing), "ETag": (NullOrUndefined Nothing), "Expiration": (NullOrUndefined Nothing), "Key": (NullOrUndefined Nothing), "Location": (NullOrUndefined Nothing), "RequestCharged": (NullOrUndefined Nothing), "SSEKMSKeyId": (NullOrUndefined Nothing), "ServerSideEncryption": (NullOrUndefined Nothing), "VersionId": (NullOrUndefined Nothing) }



newtype CompleteMultipartUploadRequest = CompleteMultipartUploadRequest 
  { "Bucket" :: (BucketName)
  , "Key" :: (ObjectKey)
  , "MultipartUpload" :: NullOrUndefined (CompletedMultipartUpload)
  , "UploadId" :: (MultipartUploadId)
  , "RequestPayer" :: NullOrUndefined (RequestPayer)
  }
derive instance newtypeCompleteMultipartUploadRequest :: Newtype CompleteMultipartUploadRequest _
derive instance repGenericCompleteMultipartUploadRequest :: Generic CompleteMultipartUploadRequest _
instance showCompleteMultipartUploadRequest :: Show CompleteMultipartUploadRequest where show = genericShow
instance decodeCompleteMultipartUploadRequest :: Decode CompleteMultipartUploadRequest where decode = genericDecode options
instance encodeCompleteMultipartUploadRequest :: Encode CompleteMultipartUploadRequest where encode = genericEncode options

-- | Constructs CompleteMultipartUploadRequest from required parameters
newCompleteMultipartUploadRequest :: BucketName -> ObjectKey -> MultipartUploadId -> CompleteMultipartUploadRequest
newCompleteMultipartUploadRequest _Bucket _Key _UploadId = CompleteMultipartUploadRequest { "Bucket": _Bucket, "Key": _Key, "UploadId": _UploadId, "MultipartUpload": (NullOrUndefined Nothing), "RequestPayer": (NullOrUndefined Nothing) }

-- | Constructs CompleteMultipartUploadRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newCompleteMultipartUploadRequest' :: BucketName -> ObjectKey -> MultipartUploadId -> ( { "Bucket" :: (BucketName) , "Key" :: (ObjectKey) , "MultipartUpload" :: NullOrUndefined (CompletedMultipartUpload) , "UploadId" :: (MultipartUploadId) , "RequestPayer" :: NullOrUndefined (RequestPayer) } -> {"Bucket" :: (BucketName) , "Key" :: (ObjectKey) , "MultipartUpload" :: NullOrUndefined (CompletedMultipartUpload) , "UploadId" :: (MultipartUploadId) , "RequestPayer" :: NullOrUndefined (RequestPayer) } ) -> CompleteMultipartUploadRequest
newCompleteMultipartUploadRequest' _Bucket _Key _UploadId customize = (CompleteMultipartUploadRequest <<< customize) { "Bucket": _Bucket, "Key": _Key, "UploadId": _UploadId, "MultipartUpload": (NullOrUndefined Nothing), "RequestPayer": (NullOrUndefined Nothing) }



newtype CompletedMultipartUpload = CompletedMultipartUpload 
  { "Parts" :: NullOrUndefined (CompletedPartList)
  }
derive instance newtypeCompletedMultipartUpload :: Newtype CompletedMultipartUpload _
derive instance repGenericCompletedMultipartUpload :: Generic CompletedMultipartUpload _
instance showCompletedMultipartUpload :: Show CompletedMultipartUpload where show = genericShow
instance decodeCompletedMultipartUpload :: Decode CompletedMultipartUpload where decode = genericDecode options
instance encodeCompletedMultipartUpload :: Encode CompletedMultipartUpload where encode = genericEncode options

-- | Constructs CompletedMultipartUpload from required parameters
newCompletedMultipartUpload :: CompletedMultipartUpload
newCompletedMultipartUpload  = CompletedMultipartUpload { "Parts": (NullOrUndefined Nothing) }

-- | Constructs CompletedMultipartUpload's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newCompletedMultipartUpload' :: ( { "Parts" :: NullOrUndefined (CompletedPartList) } -> {"Parts" :: NullOrUndefined (CompletedPartList) } ) -> CompletedMultipartUpload
newCompletedMultipartUpload'  customize = (CompletedMultipartUpload <<< customize) { "Parts": (NullOrUndefined Nothing) }



newtype CompletedPart = CompletedPart 
  { "ETag" :: NullOrUndefined (ETag)
  , "PartNumber" :: NullOrUndefined (PartNumber)
  }
derive instance newtypeCompletedPart :: Newtype CompletedPart _
derive instance repGenericCompletedPart :: Generic CompletedPart _
instance showCompletedPart :: Show CompletedPart where show = genericShow
instance decodeCompletedPart :: Decode CompletedPart where decode = genericDecode options
instance encodeCompletedPart :: Encode CompletedPart where encode = genericEncode options

-- | Constructs CompletedPart from required parameters
newCompletedPart :: CompletedPart
newCompletedPart  = CompletedPart { "ETag": (NullOrUndefined Nothing), "PartNumber": (NullOrUndefined Nothing) }

-- | Constructs CompletedPart's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newCompletedPart' :: ( { "ETag" :: NullOrUndefined (ETag) , "PartNumber" :: NullOrUndefined (PartNumber) } -> {"ETag" :: NullOrUndefined (ETag) , "PartNumber" :: NullOrUndefined (PartNumber) } ) -> CompletedPart
newCompletedPart'  customize = (CompletedPart <<< customize) { "ETag": (NullOrUndefined Nothing), "PartNumber": (NullOrUndefined Nothing) }



newtype CompletedPartList = CompletedPartList (Array CompletedPart)
derive instance newtypeCompletedPartList :: Newtype CompletedPartList _
derive instance repGenericCompletedPartList :: Generic CompletedPartList _
instance showCompletedPartList :: Show CompletedPartList where show = genericShow
instance decodeCompletedPartList :: Decode CompletedPartList where decode = genericDecode options
instance encodeCompletedPartList :: Encode CompletedPartList where encode = genericEncode options



newtype Condition = Condition 
  { "HttpErrorCodeReturnedEquals" :: NullOrUndefined (HttpErrorCodeReturnedEquals)
  , "KeyPrefixEquals" :: NullOrUndefined (KeyPrefixEquals)
  }
derive instance newtypeCondition :: Newtype Condition _
derive instance repGenericCondition :: Generic Condition _
instance showCondition :: Show Condition where show = genericShow
instance decodeCondition :: Decode Condition where decode = genericDecode options
instance encodeCondition :: Encode Condition where encode = genericEncode options

-- | Constructs Condition from required parameters
newCondition :: Condition
newCondition  = Condition { "HttpErrorCodeReturnedEquals": (NullOrUndefined Nothing), "KeyPrefixEquals": (NullOrUndefined Nothing) }

-- | Constructs Condition's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newCondition' :: ( { "HttpErrorCodeReturnedEquals" :: NullOrUndefined (HttpErrorCodeReturnedEquals) , "KeyPrefixEquals" :: NullOrUndefined (KeyPrefixEquals) } -> {"HttpErrorCodeReturnedEquals" :: NullOrUndefined (HttpErrorCodeReturnedEquals) , "KeyPrefixEquals" :: NullOrUndefined (KeyPrefixEquals) } ) -> Condition
newCondition'  customize = (Condition <<< customize) { "HttpErrorCodeReturnedEquals": (NullOrUndefined Nothing), "KeyPrefixEquals": (NullOrUndefined Nothing) }



newtype ConfirmRemoveSelfBucketAccess = ConfirmRemoveSelfBucketAccess Boolean
derive instance newtypeConfirmRemoveSelfBucketAccess :: Newtype ConfirmRemoveSelfBucketAccess _
derive instance repGenericConfirmRemoveSelfBucketAccess :: Generic ConfirmRemoveSelfBucketAccess _
instance showConfirmRemoveSelfBucketAccess :: Show ConfirmRemoveSelfBucketAccess where show = genericShow
instance decodeConfirmRemoveSelfBucketAccess :: Decode ConfirmRemoveSelfBucketAccess where decode = genericDecode options
instance encodeConfirmRemoveSelfBucketAccess :: Encode ConfirmRemoveSelfBucketAccess where encode = genericEncode options



newtype ContentDisposition = ContentDisposition String
derive instance newtypeContentDisposition :: Newtype ContentDisposition _
derive instance repGenericContentDisposition :: Generic ContentDisposition _
instance showContentDisposition :: Show ContentDisposition where show = genericShow
instance decodeContentDisposition :: Decode ContentDisposition where decode = genericDecode options
instance encodeContentDisposition :: Encode ContentDisposition where encode = genericEncode options



newtype ContentEncoding = ContentEncoding String
derive instance newtypeContentEncoding :: Newtype ContentEncoding _
derive instance repGenericContentEncoding :: Generic ContentEncoding _
instance showContentEncoding :: Show ContentEncoding where show = genericShow
instance decodeContentEncoding :: Decode ContentEncoding where decode = genericDecode options
instance encodeContentEncoding :: Encode ContentEncoding where encode = genericEncode options



newtype ContentLanguage = ContentLanguage String
derive instance newtypeContentLanguage :: Newtype ContentLanguage _
derive instance repGenericContentLanguage :: Generic ContentLanguage _
instance showContentLanguage :: Show ContentLanguage where show = genericShow
instance decodeContentLanguage :: Decode ContentLanguage where decode = genericDecode options
instance encodeContentLanguage :: Encode ContentLanguage where encode = genericEncode options



newtype ContentLength = ContentLength Number
derive instance newtypeContentLength :: Newtype ContentLength _
derive instance repGenericContentLength :: Generic ContentLength _
instance showContentLength :: Show ContentLength where show = genericShow
instance decodeContentLength :: Decode ContentLength where decode = genericDecode options
instance encodeContentLength :: Encode ContentLength where encode = genericEncode options



newtype ContentMD5 = ContentMD5 String
derive instance newtypeContentMD5 :: Newtype ContentMD5 _
derive instance repGenericContentMD5 :: Generic ContentMD5 _
instance showContentMD5 :: Show ContentMD5 where show = genericShow
instance decodeContentMD5 :: Decode ContentMD5 where decode = genericDecode options
instance encodeContentMD5 :: Encode ContentMD5 where encode = genericEncode options



newtype ContentRange = ContentRange String
derive instance newtypeContentRange :: Newtype ContentRange _
derive instance repGenericContentRange :: Generic ContentRange _
instance showContentRange :: Show ContentRange where show = genericShow
instance decodeContentRange :: Decode ContentRange where decode = genericDecode options
instance encodeContentRange :: Encode ContentRange where encode = genericEncode options



newtype ContentType = ContentType String
derive instance newtypeContentType :: Newtype ContentType _
derive instance repGenericContentType :: Generic ContentType _
instance showContentType :: Show ContentType where show = genericShow
instance decodeContentType :: Decode ContentType where decode = genericDecode options
instance encodeContentType :: Encode ContentType where encode = genericEncode options



newtype CopyObjectOutput = CopyObjectOutput 
  { "CopyObjectResult" :: NullOrUndefined (CopyObjectResult)
  , "Expiration" :: NullOrUndefined (Expiration)
  , "CopySourceVersionId" :: NullOrUndefined (CopySourceVersionId)
  , "VersionId" :: NullOrUndefined (ObjectVersionId)
  , "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption)
  , "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm)
  , "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5)
  , "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId)
  , "RequestCharged" :: NullOrUndefined (RequestCharged)
  }
derive instance newtypeCopyObjectOutput :: Newtype CopyObjectOutput _
derive instance repGenericCopyObjectOutput :: Generic CopyObjectOutput _
instance showCopyObjectOutput :: Show CopyObjectOutput where show = genericShow
instance decodeCopyObjectOutput :: Decode CopyObjectOutput where decode = genericDecode options
instance encodeCopyObjectOutput :: Encode CopyObjectOutput where encode = genericEncode options

-- | Constructs CopyObjectOutput from required parameters
newCopyObjectOutput :: CopyObjectOutput
newCopyObjectOutput  = CopyObjectOutput { "CopyObjectResult": (NullOrUndefined Nothing), "CopySourceVersionId": (NullOrUndefined Nothing), "Expiration": (NullOrUndefined Nothing), "RequestCharged": (NullOrUndefined Nothing), "SSECustomerAlgorithm": (NullOrUndefined Nothing), "SSECustomerKeyMD5": (NullOrUndefined Nothing), "SSEKMSKeyId": (NullOrUndefined Nothing), "ServerSideEncryption": (NullOrUndefined Nothing), "VersionId": (NullOrUndefined Nothing) }

-- | Constructs CopyObjectOutput's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newCopyObjectOutput' :: ( { "CopyObjectResult" :: NullOrUndefined (CopyObjectResult) , "Expiration" :: NullOrUndefined (Expiration) , "CopySourceVersionId" :: NullOrUndefined (CopySourceVersionId) , "VersionId" :: NullOrUndefined (ObjectVersionId) , "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption) , "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm) , "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5) , "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId) , "RequestCharged" :: NullOrUndefined (RequestCharged) } -> {"CopyObjectResult" :: NullOrUndefined (CopyObjectResult) , "Expiration" :: NullOrUndefined (Expiration) , "CopySourceVersionId" :: NullOrUndefined (CopySourceVersionId) , "VersionId" :: NullOrUndefined (ObjectVersionId) , "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption) , "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm) , "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5) , "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId) , "RequestCharged" :: NullOrUndefined (RequestCharged) } ) -> CopyObjectOutput
newCopyObjectOutput'  customize = (CopyObjectOutput <<< customize) { "CopyObjectResult": (NullOrUndefined Nothing), "CopySourceVersionId": (NullOrUndefined Nothing), "Expiration": (NullOrUndefined Nothing), "RequestCharged": (NullOrUndefined Nothing), "SSECustomerAlgorithm": (NullOrUndefined Nothing), "SSECustomerKeyMD5": (NullOrUndefined Nothing), "SSEKMSKeyId": (NullOrUndefined Nothing), "ServerSideEncryption": (NullOrUndefined Nothing), "VersionId": (NullOrUndefined Nothing) }



newtype CopyObjectRequest = CopyObjectRequest 
  { "ACL" :: NullOrUndefined (ObjectCannedACL)
  , "Bucket" :: (BucketName)
  , "CacheControl" :: NullOrUndefined (CacheControl)
  , "ContentDisposition" :: NullOrUndefined (ContentDisposition)
  , "ContentEncoding" :: NullOrUndefined (ContentEncoding)
  , "ContentLanguage" :: NullOrUndefined (ContentLanguage)
  , "ContentType" :: NullOrUndefined (ContentType)
  , "CopySource" :: (CopySource)
  , "CopySourceIfMatch" :: NullOrUndefined (CopySourceIfMatch)
  , "CopySourceIfModifiedSince" :: NullOrUndefined (CopySourceIfModifiedSince)
  , "CopySourceIfNoneMatch" :: NullOrUndefined (CopySourceIfNoneMatch)
  , "CopySourceIfUnmodifiedSince" :: NullOrUndefined (CopySourceIfUnmodifiedSince)
  , "Expires" :: NullOrUndefined (Expires)
  , "GrantFullControl" :: NullOrUndefined (GrantFullControl)
  , "GrantRead" :: NullOrUndefined (GrantRead)
  , "GrantReadACP" :: NullOrUndefined (GrantReadACP)
  , "GrantWriteACP" :: NullOrUndefined (GrantWriteACP)
  , "Key" :: (ObjectKey)
  , "Metadata" :: NullOrUndefined (Metadata)
  , "MetadataDirective" :: NullOrUndefined (MetadataDirective)
  , "TaggingDirective" :: NullOrUndefined (TaggingDirective)
  , "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption)
  , "StorageClass" :: NullOrUndefined (StorageClass)
  , "WebsiteRedirectLocation" :: NullOrUndefined (WebsiteRedirectLocation)
  , "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm)
  , "SSECustomerKey" :: NullOrUndefined (SSECustomerKey)
  , "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5)
  , "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId)
  , "CopySourceSSECustomerAlgorithm" :: NullOrUndefined (CopySourceSSECustomerAlgorithm)
  , "CopySourceSSECustomerKey" :: NullOrUndefined (CopySourceSSECustomerKey)
  , "CopySourceSSECustomerKeyMD5" :: NullOrUndefined (CopySourceSSECustomerKeyMD5)
  , "RequestPayer" :: NullOrUndefined (RequestPayer)
  , "Tagging" :: NullOrUndefined (TaggingHeader)
  }
derive instance newtypeCopyObjectRequest :: Newtype CopyObjectRequest _
derive instance repGenericCopyObjectRequest :: Generic CopyObjectRequest _
instance showCopyObjectRequest :: Show CopyObjectRequest where show = genericShow
instance decodeCopyObjectRequest :: Decode CopyObjectRequest where decode = genericDecode options
instance encodeCopyObjectRequest :: Encode CopyObjectRequest where encode = genericEncode options

-- | Constructs CopyObjectRequest from required parameters
newCopyObjectRequest :: BucketName -> CopySource -> ObjectKey -> CopyObjectRequest
newCopyObjectRequest _Bucket _CopySource _Key = CopyObjectRequest { "Bucket": _Bucket, "CopySource": _CopySource, "Key": _Key, "ACL": (NullOrUndefined Nothing), "CacheControl": (NullOrUndefined Nothing), "ContentDisposition": (NullOrUndefined Nothing), "ContentEncoding": (NullOrUndefined Nothing), "ContentLanguage": (NullOrUndefined Nothing), "ContentType": (NullOrUndefined Nothing), "CopySourceIfMatch": (NullOrUndefined Nothing), "CopySourceIfModifiedSince": (NullOrUndefined Nothing), "CopySourceIfNoneMatch": (NullOrUndefined Nothing), "CopySourceIfUnmodifiedSince": (NullOrUndefined Nothing), "CopySourceSSECustomerAlgorithm": (NullOrUndefined Nothing), "CopySourceSSECustomerKey": (NullOrUndefined Nothing), "CopySourceSSECustomerKeyMD5": (NullOrUndefined Nothing), "Expires": (NullOrUndefined Nothing), "GrantFullControl": (NullOrUndefined Nothing), "GrantRead": (NullOrUndefined Nothing), "GrantReadACP": (NullOrUndefined Nothing), "GrantWriteACP": (NullOrUndefined Nothing), "Metadata": (NullOrUndefined Nothing), "MetadataDirective": (NullOrUndefined Nothing), "RequestPayer": (NullOrUndefined Nothing), "SSECustomerAlgorithm": (NullOrUndefined Nothing), "SSECustomerKey": (NullOrUndefined Nothing), "SSECustomerKeyMD5": (NullOrUndefined Nothing), "SSEKMSKeyId": (NullOrUndefined Nothing), "ServerSideEncryption": (NullOrUndefined Nothing), "StorageClass": (NullOrUndefined Nothing), "Tagging": (NullOrUndefined Nothing), "TaggingDirective": (NullOrUndefined Nothing), "WebsiteRedirectLocation": (NullOrUndefined Nothing) }

-- | Constructs CopyObjectRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newCopyObjectRequest' :: BucketName -> CopySource -> ObjectKey -> ( { "ACL" :: NullOrUndefined (ObjectCannedACL) , "Bucket" :: (BucketName) , "CacheControl" :: NullOrUndefined (CacheControl) , "ContentDisposition" :: NullOrUndefined (ContentDisposition) , "ContentEncoding" :: NullOrUndefined (ContentEncoding) , "ContentLanguage" :: NullOrUndefined (ContentLanguage) , "ContentType" :: NullOrUndefined (ContentType) , "CopySource" :: (CopySource) , "CopySourceIfMatch" :: NullOrUndefined (CopySourceIfMatch) , "CopySourceIfModifiedSince" :: NullOrUndefined (CopySourceIfModifiedSince) , "CopySourceIfNoneMatch" :: NullOrUndefined (CopySourceIfNoneMatch) , "CopySourceIfUnmodifiedSince" :: NullOrUndefined (CopySourceIfUnmodifiedSince) , "Expires" :: NullOrUndefined (Expires) , "GrantFullControl" :: NullOrUndefined (GrantFullControl) , "GrantRead" :: NullOrUndefined (GrantRead) , "GrantReadACP" :: NullOrUndefined (GrantReadACP) , "GrantWriteACP" :: NullOrUndefined (GrantWriteACP) , "Key" :: (ObjectKey) , "Metadata" :: NullOrUndefined (Metadata) , "MetadataDirective" :: NullOrUndefined (MetadataDirective) , "TaggingDirective" :: NullOrUndefined (TaggingDirective) , "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption) , "StorageClass" :: NullOrUndefined (StorageClass) , "WebsiteRedirectLocation" :: NullOrUndefined (WebsiteRedirectLocation) , "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm) , "SSECustomerKey" :: NullOrUndefined (SSECustomerKey) , "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5) , "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId) , "CopySourceSSECustomerAlgorithm" :: NullOrUndefined (CopySourceSSECustomerAlgorithm) , "CopySourceSSECustomerKey" :: NullOrUndefined (CopySourceSSECustomerKey) , "CopySourceSSECustomerKeyMD5" :: NullOrUndefined (CopySourceSSECustomerKeyMD5) , "RequestPayer" :: NullOrUndefined (RequestPayer) , "Tagging" :: NullOrUndefined (TaggingHeader) } -> {"ACL" :: NullOrUndefined (ObjectCannedACL) , "Bucket" :: (BucketName) , "CacheControl" :: NullOrUndefined (CacheControl) , "ContentDisposition" :: NullOrUndefined (ContentDisposition) , "ContentEncoding" :: NullOrUndefined (ContentEncoding) , "ContentLanguage" :: NullOrUndefined (ContentLanguage) , "ContentType" :: NullOrUndefined (ContentType) , "CopySource" :: (CopySource) , "CopySourceIfMatch" :: NullOrUndefined (CopySourceIfMatch) , "CopySourceIfModifiedSince" :: NullOrUndefined (CopySourceIfModifiedSince) , "CopySourceIfNoneMatch" :: NullOrUndefined (CopySourceIfNoneMatch) , "CopySourceIfUnmodifiedSince" :: NullOrUndefined (CopySourceIfUnmodifiedSince) , "Expires" :: NullOrUndefined (Expires) , "GrantFullControl" :: NullOrUndefined (GrantFullControl) , "GrantRead" :: NullOrUndefined (GrantRead) , "GrantReadACP" :: NullOrUndefined (GrantReadACP) , "GrantWriteACP" :: NullOrUndefined (GrantWriteACP) , "Key" :: (ObjectKey) , "Metadata" :: NullOrUndefined (Metadata) , "MetadataDirective" :: NullOrUndefined (MetadataDirective) , "TaggingDirective" :: NullOrUndefined (TaggingDirective) , "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption) , "StorageClass" :: NullOrUndefined (StorageClass) , "WebsiteRedirectLocation" :: NullOrUndefined (WebsiteRedirectLocation) , "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm) , "SSECustomerKey" :: NullOrUndefined (SSECustomerKey) , "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5) , "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId) , "CopySourceSSECustomerAlgorithm" :: NullOrUndefined (CopySourceSSECustomerAlgorithm) , "CopySourceSSECustomerKey" :: NullOrUndefined (CopySourceSSECustomerKey) , "CopySourceSSECustomerKeyMD5" :: NullOrUndefined (CopySourceSSECustomerKeyMD5) , "RequestPayer" :: NullOrUndefined (RequestPayer) , "Tagging" :: NullOrUndefined (TaggingHeader) } ) -> CopyObjectRequest
newCopyObjectRequest' _Bucket _CopySource _Key customize = (CopyObjectRequest <<< customize) { "Bucket": _Bucket, "CopySource": _CopySource, "Key": _Key, "ACL": (NullOrUndefined Nothing), "CacheControl": (NullOrUndefined Nothing), "ContentDisposition": (NullOrUndefined Nothing), "ContentEncoding": (NullOrUndefined Nothing), "ContentLanguage": (NullOrUndefined Nothing), "ContentType": (NullOrUndefined Nothing), "CopySourceIfMatch": (NullOrUndefined Nothing), "CopySourceIfModifiedSince": (NullOrUndefined Nothing), "CopySourceIfNoneMatch": (NullOrUndefined Nothing), "CopySourceIfUnmodifiedSince": (NullOrUndefined Nothing), "CopySourceSSECustomerAlgorithm": (NullOrUndefined Nothing), "CopySourceSSECustomerKey": (NullOrUndefined Nothing), "CopySourceSSECustomerKeyMD5": (NullOrUndefined Nothing), "Expires": (NullOrUndefined Nothing), "GrantFullControl": (NullOrUndefined Nothing), "GrantRead": (NullOrUndefined Nothing), "GrantReadACP": (NullOrUndefined Nothing), "GrantWriteACP": (NullOrUndefined Nothing), "Metadata": (NullOrUndefined Nothing), "MetadataDirective": (NullOrUndefined Nothing), "RequestPayer": (NullOrUndefined Nothing), "SSECustomerAlgorithm": (NullOrUndefined Nothing), "SSECustomerKey": (NullOrUndefined Nothing), "SSECustomerKeyMD5": (NullOrUndefined Nothing), "SSEKMSKeyId": (NullOrUndefined Nothing), "ServerSideEncryption": (NullOrUndefined Nothing), "StorageClass": (NullOrUndefined Nothing), "Tagging": (NullOrUndefined Nothing), "TaggingDirective": (NullOrUndefined Nothing), "WebsiteRedirectLocation": (NullOrUndefined Nothing) }



newtype CopyObjectResult = CopyObjectResult 
  { "ETag" :: NullOrUndefined (ETag)
  , "LastModified" :: NullOrUndefined (LastModified)
  }
derive instance newtypeCopyObjectResult :: Newtype CopyObjectResult _
derive instance repGenericCopyObjectResult :: Generic CopyObjectResult _
instance showCopyObjectResult :: Show CopyObjectResult where show = genericShow
instance decodeCopyObjectResult :: Decode CopyObjectResult where decode = genericDecode options
instance encodeCopyObjectResult :: Encode CopyObjectResult where encode = genericEncode options

-- | Constructs CopyObjectResult from required parameters
newCopyObjectResult :: CopyObjectResult
newCopyObjectResult  = CopyObjectResult { "ETag": (NullOrUndefined Nothing), "LastModified": (NullOrUndefined Nothing) }

-- | Constructs CopyObjectResult's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newCopyObjectResult' :: ( { "ETag" :: NullOrUndefined (ETag) , "LastModified" :: NullOrUndefined (LastModified) } -> {"ETag" :: NullOrUndefined (ETag) , "LastModified" :: NullOrUndefined (LastModified) } ) -> CopyObjectResult
newCopyObjectResult'  customize = (CopyObjectResult <<< customize) { "ETag": (NullOrUndefined Nothing), "LastModified": (NullOrUndefined Nothing) }



newtype CopyPartResult = CopyPartResult 
  { "ETag" :: NullOrUndefined (ETag)
  , "LastModified" :: NullOrUndefined (LastModified)
  }
derive instance newtypeCopyPartResult :: Newtype CopyPartResult _
derive instance repGenericCopyPartResult :: Generic CopyPartResult _
instance showCopyPartResult :: Show CopyPartResult where show = genericShow
instance decodeCopyPartResult :: Decode CopyPartResult where decode = genericDecode options
instance encodeCopyPartResult :: Encode CopyPartResult where encode = genericEncode options

-- | Constructs CopyPartResult from required parameters
newCopyPartResult :: CopyPartResult
newCopyPartResult  = CopyPartResult { "ETag": (NullOrUndefined Nothing), "LastModified": (NullOrUndefined Nothing) }

-- | Constructs CopyPartResult's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newCopyPartResult' :: ( { "ETag" :: NullOrUndefined (ETag) , "LastModified" :: NullOrUndefined (LastModified) } -> {"ETag" :: NullOrUndefined (ETag) , "LastModified" :: NullOrUndefined (LastModified) } ) -> CopyPartResult
newCopyPartResult'  customize = (CopyPartResult <<< customize) { "ETag": (NullOrUndefined Nothing), "LastModified": (NullOrUndefined Nothing) }



newtype CopySource = CopySource String
derive instance newtypeCopySource :: Newtype CopySource _
derive instance repGenericCopySource :: Generic CopySource _
instance showCopySource :: Show CopySource where show = genericShow
instance decodeCopySource :: Decode CopySource where decode = genericDecode options
instance encodeCopySource :: Encode CopySource where encode = genericEncode options



newtype CopySourceIfMatch = CopySourceIfMatch String
derive instance newtypeCopySourceIfMatch :: Newtype CopySourceIfMatch _
derive instance repGenericCopySourceIfMatch :: Generic CopySourceIfMatch _
instance showCopySourceIfMatch :: Show CopySourceIfMatch where show = genericShow
instance decodeCopySourceIfMatch :: Decode CopySourceIfMatch where decode = genericDecode options
instance encodeCopySourceIfMatch :: Encode CopySourceIfMatch where encode = genericEncode options



newtype CopySourceIfModifiedSince = CopySourceIfModifiedSince Types.Timestamp
derive instance newtypeCopySourceIfModifiedSince :: Newtype CopySourceIfModifiedSince _
derive instance repGenericCopySourceIfModifiedSince :: Generic CopySourceIfModifiedSince _
instance showCopySourceIfModifiedSince :: Show CopySourceIfModifiedSince where show = genericShow
instance decodeCopySourceIfModifiedSince :: Decode CopySourceIfModifiedSince where decode = genericDecode options
instance encodeCopySourceIfModifiedSince :: Encode CopySourceIfModifiedSince where encode = genericEncode options



newtype CopySourceIfNoneMatch = CopySourceIfNoneMatch String
derive instance newtypeCopySourceIfNoneMatch :: Newtype CopySourceIfNoneMatch _
derive instance repGenericCopySourceIfNoneMatch :: Generic CopySourceIfNoneMatch _
instance showCopySourceIfNoneMatch :: Show CopySourceIfNoneMatch where show = genericShow
instance decodeCopySourceIfNoneMatch :: Decode CopySourceIfNoneMatch where decode = genericDecode options
instance encodeCopySourceIfNoneMatch :: Encode CopySourceIfNoneMatch where encode = genericEncode options



newtype CopySourceIfUnmodifiedSince = CopySourceIfUnmodifiedSince Types.Timestamp
derive instance newtypeCopySourceIfUnmodifiedSince :: Newtype CopySourceIfUnmodifiedSince _
derive instance repGenericCopySourceIfUnmodifiedSince :: Generic CopySourceIfUnmodifiedSince _
instance showCopySourceIfUnmodifiedSince :: Show CopySourceIfUnmodifiedSince where show = genericShow
instance decodeCopySourceIfUnmodifiedSince :: Decode CopySourceIfUnmodifiedSince where decode = genericDecode options
instance encodeCopySourceIfUnmodifiedSince :: Encode CopySourceIfUnmodifiedSince where encode = genericEncode options



newtype CopySourceRange = CopySourceRange String
derive instance newtypeCopySourceRange :: Newtype CopySourceRange _
derive instance repGenericCopySourceRange :: Generic CopySourceRange _
instance showCopySourceRange :: Show CopySourceRange where show = genericShow
instance decodeCopySourceRange :: Decode CopySourceRange where decode = genericDecode options
instance encodeCopySourceRange :: Encode CopySourceRange where encode = genericEncode options



newtype CopySourceSSECustomerAlgorithm = CopySourceSSECustomerAlgorithm String
derive instance newtypeCopySourceSSECustomerAlgorithm :: Newtype CopySourceSSECustomerAlgorithm _
derive instance repGenericCopySourceSSECustomerAlgorithm :: Generic CopySourceSSECustomerAlgorithm _
instance showCopySourceSSECustomerAlgorithm :: Show CopySourceSSECustomerAlgorithm where show = genericShow
instance decodeCopySourceSSECustomerAlgorithm :: Decode CopySourceSSECustomerAlgorithm where decode = genericDecode options
instance encodeCopySourceSSECustomerAlgorithm :: Encode CopySourceSSECustomerAlgorithm where encode = genericEncode options



newtype CopySourceSSECustomerKey = CopySourceSSECustomerKey String
derive instance newtypeCopySourceSSECustomerKey :: Newtype CopySourceSSECustomerKey _
derive instance repGenericCopySourceSSECustomerKey :: Generic CopySourceSSECustomerKey _
instance showCopySourceSSECustomerKey :: Show CopySourceSSECustomerKey where show = genericShow
instance decodeCopySourceSSECustomerKey :: Decode CopySourceSSECustomerKey where decode = genericDecode options
instance encodeCopySourceSSECustomerKey :: Encode CopySourceSSECustomerKey where encode = genericEncode options



newtype CopySourceSSECustomerKeyMD5 = CopySourceSSECustomerKeyMD5 String
derive instance newtypeCopySourceSSECustomerKeyMD5 :: Newtype CopySourceSSECustomerKeyMD5 _
derive instance repGenericCopySourceSSECustomerKeyMD5 :: Generic CopySourceSSECustomerKeyMD5 _
instance showCopySourceSSECustomerKeyMD5 :: Show CopySourceSSECustomerKeyMD5 where show = genericShow
instance decodeCopySourceSSECustomerKeyMD5 :: Decode CopySourceSSECustomerKeyMD5 where decode = genericDecode options
instance encodeCopySourceSSECustomerKeyMD5 :: Encode CopySourceSSECustomerKeyMD5 where encode = genericEncode options



newtype CopySourceVersionId = CopySourceVersionId String
derive instance newtypeCopySourceVersionId :: Newtype CopySourceVersionId _
derive instance repGenericCopySourceVersionId :: Generic CopySourceVersionId _
instance showCopySourceVersionId :: Show CopySourceVersionId where show = genericShow
instance decodeCopySourceVersionId :: Decode CopySourceVersionId where decode = genericDecode options
instance encodeCopySourceVersionId :: Encode CopySourceVersionId where encode = genericEncode options



newtype CreateBucketConfiguration = CreateBucketConfiguration 
  { "LocationConstraint" :: NullOrUndefined (BucketLocationConstraint)
  }
derive instance newtypeCreateBucketConfiguration :: Newtype CreateBucketConfiguration _
derive instance repGenericCreateBucketConfiguration :: Generic CreateBucketConfiguration _
instance showCreateBucketConfiguration :: Show CreateBucketConfiguration where show = genericShow
instance decodeCreateBucketConfiguration :: Decode CreateBucketConfiguration where decode = genericDecode options
instance encodeCreateBucketConfiguration :: Encode CreateBucketConfiguration where encode = genericEncode options

-- | Constructs CreateBucketConfiguration from required parameters
newCreateBucketConfiguration :: CreateBucketConfiguration
newCreateBucketConfiguration  = CreateBucketConfiguration { "LocationConstraint": (NullOrUndefined Nothing) }

-- | Constructs CreateBucketConfiguration's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newCreateBucketConfiguration' :: ( { "LocationConstraint" :: NullOrUndefined (BucketLocationConstraint) } -> {"LocationConstraint" :: NullOrUndefined (BucketLocationConstraint) } ) -> CreateBucketConfiguration
newCreateBucketConfiguration'  customize = (CreateBucketConfiguration <<< customize) { "LocationConstraint": (NullOrUndefined Nothing) }



newtype CreateBucketOutput = CreateBucketOutput 
  { "Location" :: NullOrUndefined (Location)
  }
derive instance newtypeCreateBucketOutput :: Newtype CreateBucketOutput _
derive instance repGenericCreateBucketOutput :: Generic CreateBucketOutput _
instance showCreateBucketOutput :: Show CreateBucketOutput where show = genericShow
instance decodeCreateBucketOutput :: Decode CreateBucketOutput where decode = genericDecode options
instance encodeCreateBucketOutput :: Encode CreateBucketOutput where encode = genericEncode options

-- | Constructs CreateBucketOutput from required parameters
newCreateBucketOutput :: CreateBucketOutput
newCreateBucketOutput  = CreateBucketOutput { "Location": (NullOrUndefined Nothing) }

-- | Constructs CreateBucketOutput's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newCreateBucketOutput' :: ( { "Location" :: NullOrUndefined (Location) } -> {"Location" :: NullOrUndefined (Location) } ) -> CreateBucketOutput
newCreateBucketOutput'  customize = (CreateBucketOutput <<< customize) { "Location": (NullOrUndefined Nothing) }



newtype CreateBucketRequest = CreateBucketRequest 
  { "ACL" :: NullOrUndefined (BucketCannedACL)
  , "Bucket" :: (BucketName)
  , "CreateBucketConfiguration" :: NullOrUndefined (CreateBucketConfiguration)
  , "GrantFullControl" :: NullOrUndefined (GrantFullControl)
  , "GrantRead" :: NullOrUndefined (GrantRead)
  , "GrantReadACP" :: NullOrUndefined (GrantReadACP)
  , "GrantWrite" :: NullOrUndefined (GrantWrite)
  , "GrantWriteACP" :: NullOrUndefined (GrantWriteACP)
  }
derive instance newtypeCreateBucketRequest :: Newtype CreateBucketRequest _
derive instance repGenericCreateBucketRequest :: Generic CreateBucketRequest _
instance showCreateBucketRequest :: Show CreateBucketRequest where show = genericShow
instance decodeCreateBucketRequest :: Decode CreateBucketRequest where decode = genericDecode options
instance encodeCreateBucketRequest :: Encode CreateBucketRequest where encode = genericEncode options

-- | Constructs CreateBucketRequest from required parameters
newCreateBucketRequest :: BucketName -> CreateBucketRequest
newCreateBucketRequest _Bucket = CreateBucketRequest { "Bucket": _Bucket, "ACL": (NullOrUndefined Nothing), "CreateBucketConfiguration": (NullOrUndefined Nothing), "GrantFullControl": (NullOrUndefined Nothing), "GrantRead": (NullOrUndefined Nothing), "GrantReadACP": (NullOrUndefined Nothing), "GrantWrite": (NullOrUndefined Nothing), "GrantWriteACP": (NullOrUndefined Nothing) }

-- | Constructs CreateBucketRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newCreateBucketRequest' :: BucketName -> ( { "ACL" :: NullOrUndefined (BucketCannedACL) , "Bucket" :: (BucketName) , "CreateBucketConfiguration" :: NullOrUndefined (CreateBucketConfiguration) , "GrantFullControl" :: NullOrUndefined (GrantFullControl) , "GrantRead" :: NullOrUndefined (GrantRead) , "GrantReadACP" :: NullOrUndefined (GrantReadACP) , "GrantWrite" :: NullOrUndefined (GrantWrite) , "GrantWriteACP" :: NullOrUndefined (GrantWriteACP) } -> {"ACL" :: NullOrUndefined (BucketCannedACL) , "Bucket" :: (BucketName) , "CreateBucketConfiguration" :: NullOrUndefined (CreateBucketConfiguration) , "GrantFullControl" :: NullOrUndefined (GrantFullControl) , "GrantRead" :: NullOrUndefined (GrantRead) , "GrantReadACP" :: NullOrUndefined (GrantReadACP) , "GrantWrite" :: NullOrUndefined (GrantWrite) , "GrantWriteACP" :: NullOrUndefined (GrantWriteACP) } ) -> CreateBucketRequest
newCreateBucketRequest' _Bucket customize = (CreateBucketRequest <<< customize) { "Bucket": _Bucket, "ACL": (NullOrUndefined Nothing), "CreateBucketConfiguration": (NullOrUndefined Nothing), "GrantFullControl": (NullOrUndefined Nothing), "GrantRead": (NullOrUndefined Nothing), "GrantReadACP": (NullOrUndefined Nothing), "GrantWrite": (NullOrUndefined Nothing), "GrantWriteACP": (NullOrUndefined Nothing) }



newtype CreateMultipartUploadOutput = CreateMultipartUploadOutput 
  { "AbortDate" :: NullOrUndefined (AbortDate)
  , "AbortRuleId" :: NullOrUndefined (AbortRuleId)
  , "Bucket" :: NullOrUndefined (BucketName)
  , "Key" :: NullOrUndefined (ObjectKey)
  , "UploadId" :: NullOrUndefined (MultipartUploadId)
  , "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption)
  , "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm)
  , "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5)
  , "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId)
  , "RequestCharged" :: NullOrUndefined (RequestCharged)
  }
derive instance newtypeCreateMultipartUploadOutput :: Newtype CreateMultipartUploadOutput _
derive instance repGenericCreateMultipartUploadOutput :: Generic CreateMultipartUploadOutput _
instance showCreateMultipartUploadOutput :: Show CreateMultipartUploadOutput where show = genericShow
instance decodeCreateMultipartUploadOutput :: Decode CreateMultipartUploadOutput where decode = genericDecode options
instance encodeCreateMultipartUploadOutput :: Encode CreateMultipartUploadOutput where encode = genericEncode options

-- | Constructs CreateMultipartUploadOutput from required parameters
newCreateMultipartUploadOutput :: CreateMultipartUploadOutput
newCreateMultipartUploadOutput  = CreateMultipartUploadOutput { "AbortDate": (NullOrUndefined Nothing), "AbortRuleId": (NullOrUndefined Nothing), "Bucket": (NullOrUndefined Nothing), "Key": (NullOrUndefined Nothing), "RequestCharged": (NullOrUndefined Nothing), "SSECustomerAlgorithm": (NullOrUndefined Nothing), "SSECustomerKeyMD5": (NullOrUndefined Nothing), "SSEKMSKeyId": (NullOrUndefined Nothing), "ServerSideEncryption": (NullOrUndefined Nothing), "UploadId": (NullOrUndefined Nothing) }

-- | Constructs CreateMultipartUploadOutput's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newCreateMultipartUploadOutput' :: ( { "AbortDate" :: NullOrUndefined (AbortDate) , "AbortRuleId" :: NullOrUndefined (AbortRuleId) , "Bucket" :: NullOrUndefined (BucketName) , "Key" :: NullOrUndefined (ObjectKey) , "UploadId" :: NullOrUndefined (MultipartUploadId) , "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption) , "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm) , "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5) , "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId) , "RequestCharged" :: NullOrUndefined (RequestCharged) } -> {"AbortDate" :: NullOrUndefined (AbortDate) , "AbortRuleId" :: NullOrUndefined (AbortRuleId) , "Bucket" :: NullOrUndefined (BucketName) , "Key" :: NullOrUndefined (ObjectKey) , "UploadId" :: NullOrUndefined (MultipartUploadId) , "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption) , "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm) , "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5) , "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId) , "RequestCharged" :: NullOrUndefined (RequestCharged) } ) -> CreateMultipartUploadOutput
newCreateMultipartUploadOutput'  customize = (CreateMultipartUploadOutput <<< customize) { "AbortDate": (NullOrUndefined Nothing), "AbortRuleId": (NullOrUndefined Nothing), "Bucket": (NullOrUndefined Nothing), "Key": (NullOrUndefined Nothing), "RequestCharged": (NullOrUndefined Nothing), "SSECustomerAlgorithm": (NullOrUndefined Nothing), "SSECustomerKeyMD5": (NullOrUndefined Nothing), "SSEKMSKeyId": (NullOrUndefined Nothing), "ServerSideEncryption": (NullOrUndefined Nothing), "UploadId": (NullOrUndefined Nothing) }



newtype CreateMultipartUploadRequest = CreateMultipartUploadRequest 
  { "ACL" :: NullOrUndefined (ObjectCannedACL)
  , "Bucket" :: (BucketName)
  , "CacheControl" :: NullOrUndefined (CacheControl)
  , "ContentDisposition" :: NullOrUndefined (ContentDisposition)
  , "ContentEncoding" :: NullOrUndefined (ContentEncoding)
  , "ContentLanguage" :: NullOrUndefined (ContentLanguage)
  , "ContentType" :: NullOrUndefined (ContentType)
  , "Expires" :: NullOrUndefined (Expires)
  , "GrantFullControl" :: NullOrUndefined (GrantFullControl)
  , "GrantRead" :: NullOrUndefined (GrantRead)
  , "GrantReadACP" :: NullOrUndefined (GrantReadACP)
  , "GrantWriteACP" :: NullOrUndefined (GrantWriteACP)
  , "Key" :: (ObjectKey)
  , "Metadata" :: NullOrUndefined (Metadata)
  , "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption)
  , "StorageClass" :: NullOrUndefined (StorageClass)
  , "WebsiteRedirectLocation" :: NullOrUndefined (WebsiteRedirectLocation)
  , "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm)
  , "SSECustomerKey" :: NullOrUndefined (SSECustomerKey)
  , "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5)
  , "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId)
  , "RequestPayer" :: NullOrUndefined (RequestPayer)
  , "Tagging" :: NullOrUndefined (TaggingHeader)
  }
derive instance newtypeCreateMultipartUploadRequest :: Newtype CreateMultipartUploadRequest _
derive instance repGenericCreateMultipartUploadRequest :: Generic CreateMultipartUploadRequest _
instance showCreateMultipartUploadRequest :: Show CreateMultipartUploadRequest where show = genericShow
instance decodeCreateMultipartUploadRequest :: Decode CreateMultipartUploadRequest where decode = genericDecode options
instance encodeCreateMultipartUploadRequest :: Encode CreateMultipartUploadRequest where encode = genericEncode options

-- | Constructs CreateMultipartUploadRequest from required parameters
newCreateMultipartUploadRequest :: BucketName -> ObjectKey -> CreateMultipartUploadRequest
newCreateMultipartUploadRequest _Bucket _Key = CreateMultipartUploadRequest { "Bucket": _Bucket, "Key": _Key, "ACL": (NullOrUndefined Nothing), "CacheControl": (NullOrUndefined Nothing), "ContentDisposition": (NullOrUndefined Nothing), "ContentEncoding": (NullOrUndefined Nothing), "ContentLanguage": (NullOrUndefined Nothing), "ContentType": (NullOrUndefined Nothing), "Expires": (NullOrUndefined Nothing), "GrantFullControl": (NullOrUndefined Nothing), "GrantRead": (NullOrUndefined Nothing), "GrantReadACP": (NullOrUndefined Nothing), "GrantWriteACP": (NullOrUndefined Nothing), "Metadata": (NullOrUndefined Nothing), "RequestPayer": (NullOrUndefined Nothing), "SSECustomerAlgorithm": (NullOrUndefined Nothing), "SSECustomerKey": (NullOrUndefined Nothing), "SSECustomerKeyMD5": (NullOrUndefined Nothing), "SSEKMSKeyId": (NullOrUndefined Nothing), "ServerSideEncryption": (NullOrUndefined Nothing), "StorageClass": (NullOrUndefined Nothing), "Tagging": (NullOrUndefined Nothing), "WebsiteRedirectLocation": (NullOrUndefined Nothing) }

-- | Constructs CreateMultipartUploadRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newCreateMultipartUploadRequest' :: BucketName -> ObjectKey -> ( { "ACL" :: NullOrUndefined (ObjectCannedACL) , "Bucket" :: (BucketName) , "CacheControl" :: NullOrUndefined (CacheControl) , "ContentDisposition" :: NullOrUndefined (ContentDisposition) , "ContentEncoding" :: NullOrUndefined (ContentEncoding) , "ContentLanguage" :: NullOrUndefined (ContentLanguage) , "ContentType" :: NullOrUndefined (ContentType) , "Expires" :: NullOrUndefined (Expires) , "GrantFullControl" :: NullOrUndefined (GrantFullControl) , "GrantRead" :: NullOrUndefined (GrantRead) , "GrantReadACP" :: NullOrUndefined (GrantReadACP) , "GrantWriteACP" :: NullOrUndefined (GrantWriteACP) , "Key" :: (ObjectKey) , "Metadata" :: NullOrUndefined (Metadata) , "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption) , "StorageClass" :: NullOrUndefined (StorageClass) , "WebsiteRedirectLocation" :: NullOrUndefined (WebsiteRedirectLocation) , "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm) , "SSECustomerKey" :: NullOrUndefined (SSECustomerKey) , "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5) , "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId) , "RequestPayer" :: NullOrUndefined (RequestPayer) , "Tagging" :: NullOrUndefined (TaggingHeader) } -> {"ACL" :: NullOrUndefined (ObjectCannedACL) , "Bucket" :: (BucketName) , "CacheControl" :: NullOrUndefined (CacheControl) , "ContentDisposition" :: NullOrUndefined (ContentDisposition) , "ContentEncoding" :: NullOrUndefined (ContentEncoding) , "ContentLanguage" :: NullOrUndefined (ContentLanguage) , "ContentType" :: NullOrUndefined (ContentType) , "Expires" :: NullOrUndefined (Expires) , "GrantFullControl" :: NullOrUndefined (GrantFullControl) , "GrantRead" :: NullOrUndefined (GrantRead) , "GrantReadACP" :: NullOrUndefined (GrantReadACP) , "GrantWriteACP" :: NullOrUndefined (GrantWriteACP) , "Key" :: (ObjectKey) , "Metadata" :: NullOrUndefined (Metadata) , "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption) , "StorageClass" :: NullOrUndefined (StorageClass) , "WebsiteRedirectLocation" :: NullOrUndefined (WebsiteRedirectLocation) , "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm) , "SSECustomerKey" :: NullOrUndefined (SSECustomerKey) , "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5) , "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId) , "RequestPayer" :: NullOrUndefined (RequestPayer) , "Tagging" :: NullOrUndefined (TaggingHeader) } ) -> CreateMultipartUploadRequest
newCreateMultipartUploadRequest' _Bucket _Key customize = (CreateMultipartUploadRequest <<< customize) { "Bucket": _Bucket, "Key": _Key, "ACL": (NullOrUndefined Nothing), "CacheControl": (NullOrUndefined Nothing), "ContentDisposition": (NullOrUndefined Nothing), "ContentEncoding": (NullOrUndefined Nothing), "ContentLanguage": (NullOrUndefined Nothing), "ContentType": (NullOrUndefined Nothing), "Expires": (NullOrUndefined Nothing), "GrantFullControl": (NullOrUndefined Nothing), "GrantRead": (NullOrUndefined Nothing), "GrantReadACP": (NullOrUndefined Nothing), "GrantWriteACP": (NullOrUndefined Nothing), "Metadata": (NullOrUndefined Nothing), "RequestPayer": (NullOrUndefined Nothing), "SSECustomerAlgorithm": (NullOrUndefined Nothing), "SSECustomerKey": (NullOrUndefined Nothing), "SSECustomerKeyMD5": (NullOrUndefined Nothing), "SSEKMSKeyId": (NullOrUndefined Nothing), "ServerSideEncryption": (NullOrUndefined Nothing), "StorageClass": (NullOrUndefined Nothing), "Tagging": (NullOrUndefined Nothing), "WebsiteRedirectLocation": (NullOrUndefined Nothing) }



newtype CreationDate = CreationDate Types.Timestamp
derive instance newtypeCreationDate :: Newtype CreationDate _
derive instance repGenericCreationDate :: Generic CreationDate _
instance showCreationDate :: Show CreationDate where show = genericShow
instance decodeCreationDate :: Decode CreationDate where decode = genericDecode options
instance encodeCreationDate :: Encode CreationDate where encode = genericEncode options



newtype Date = Date Types.Timestamp
derive instance newtypeDate :: Newtype Date _
derive instance repGenericDate :: Generic Date _
instance showDate :: Show Date where show = genericShow
instance decodeDate :: Decode Date where decode = genericDecode options
instance encodeDate :: Encode Date where encode = genericEncode options



newtype Days = Days Int
derive instance newtypeDays :: Newtype Days _
derive instance repGenericDays :: Generic Days _
instance showDays :: Show Days where show = genericShow
instance decodeDays :: Decode Days where decode = genericDecode options
instance encodeDays :: Encode Days where encode = genericEncode options



newtype DaysAfterInitiation = DaysAfterInitiation Int
derive instance newtypeDaysAfterInitiation :: Newtype DaysAfterInitiation _
derive instance repGenericDaysAfterInitiation :: Generic DaysAfterInitiation _
instance showDaysAfterInitiation :: Show DaysAfterInitiation where show = genericShow
instance decodeDaysAfterInitiation :: Decode DaysAfterInitiation where decode = genericDecode options
instance encodeDaysAfterInitiation :: Encode DaysAfterInitiation where encode = genericEncode options



newtype Delete = Delete 
  { "Objects" :: (ObjectIdentifierList)
  , "Quiet" :: NullOrUndefined (Quiet)
  }
derive instance newtypeDelete :: Newtype Delete _
derive instance repGenericDelete :: Generic Delete _
instance showDelete :: Show Delete where show = genericShow
instance decodeDelete :: Decode Delete where decode = genericDecode options
instance encodeDelete :: Encode Delete where encode = genericEncode options

-- | Constructs Delete from required parameters
newDelete :: ObjectIdentifierList -> Delete
newDelete _Objects = Delete { "Objects": _Objects, "Quiet": (NullOrUndefined Nothing) }

-- | Constructs Delete's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newDelete' :: ObjectIdentifierList -> ( { "Objects" :: (ObjectIdentifierList) , "Quiet" :: NullOrUndefined (Quiet) } -> {"Objects" :: (ObjectIdentifierList) , "Quiet" :: NullOrUndefined (Quiet) } ) -> Delete
newDelete' _Objects customize = (Delete <<< customize) { "Objects": _Objects, "Quiet": (NullOrUndefined Nothing) }



newtype DeleteBucketAnalyticsConfigurationRequest = DeleteBucketAnalyticsConfigurationRequest 
  { "Bucket" :: (BucketName)
  , "Id" :: (AnalyticsId)
  }
derive instance newtypeDeleteBucketAnalyticsConfigurationRequest :: Newtype DeleteBucketAnalyticsConfigurationRequest _
derive instance repGenericDeleteBucketAnalyticsConfigurationRequest :: Generic DeleteBucketAnalyticsConfigurationRequest _
instance showDeleteBucketAnalyticsConfigurationRequest :: Show DeleteBucketAnalyticsConfigurationRequest where show = genericShow
instance decodeDeleteBucketAnalyticsConfigurationRequest :: Decode DeleteBucketAnalyticsConfigurationRequest where decode = genericDecode options
instance encodeDeleteBucketAnalyticsConfigurationRequest :: Encode DeleteBucketAnalyticsConfigurationRequest where encode = genericEncode options

-- | Constructs DeleteBucketAnalyticsConfigurationRequest from required parameters
newDeleteBucketAnalyticsConfigurationRequest :: BucketName -> AnalyticsId -> DeleteBucketAnalyticsConfigurationRequest
newDeleteBucketAnalyticsConfigurationRequest _Bucket _Id = DeleteBucketAnalyticsConfigurationRequest { "Bucket": _Bucket, "Id": _Id }

-- | Constructs DeleteBucketAnalyticsConfigurationRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newDeleteBucketAnalyticsConfigurationRequest' :: BucketName -> AnalyticsId -> ( { "Bucket" :: (BucketName) , "Id" :: (AnalyticsId) } -> {"Bucket" :: (BucketName) , "Id" :: (AnalyticsId) } ) -> DeleteBucketAnalyticsConfigurationRequest
newDeleteBucketAnalyticsConfigurationRequest' _Bucket _Id customize = (DeleteBucketAnalyticsConfigurationRequest <<< customize) { "Bucket": _Bucket, "Id": _Id }



newtype DeleteBucketCorsRequest = DeleteBucketCorsRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeDeleteBucketCorsRequest :: Newtype DeleteBucketCorsRequest _
derive instance repGenericDeleteBucketCorsRequest :: Generic DeleteBucketCorsRequest _
instance showDeleteBucketCorsRequest :: Show DeleteBucketCorsRequest where show = genericShow
instance decodeDeleteBucketCorsRequest :: Decode DeleteBucketCorsRequest where decode = genericDecode options
instance encodeDeleteBucketCorsRequest :: Encode DeleteBucketCorsRequest where encode = genericEncode options

-- | Constructs DeleteBucketCorsRequest from required parameters
newDeleteBucketCorsRequest :: BucketName -> DeleteBucketCorsRequest
newDeleteBucketCorsRequest _Bucket = DeleteBucketCorsRequest { "Bucket": _Bucket }

-- | Constructs DeleteBucketCorsRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newDeleteBucketCorsRequest' :: BucketName -> ( { "Bucket" :: (BucketName) } -> {"Bucket" :: (BucketName) } ) -> DeleteBucketCorsRequest
newDeleteBucketCorsRequest' _Bucket customize = (DeleteBucketCorsRequest <<< customize) { "Bucket": _Bucket }



newtype DeleteBucketEncryptionRequest = DeleteBucketEncryptionRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeDeleteBucketEncryptionRequest :: Newtype DeleteBucketEncryptionRequest _
derive instance repGenericDeleteBucketEncryptionRequest :: Generic DeleteBucketEncryptionRequest _
instance showDeleteBucketEncryptionRequest :: Show DeleteBucketEncryptionRequest where show = genericShow
instance decodeDeleteBucketEncryptionRequest :: Decode DeleteBucketEncryptionRequest where decode = genericDecode options
instance encodeDeleteBucketEncryptionRequest :: Encode DeleteBucketEncryptionRequest where encode = genericEncode options

-- | Constructs DeleteBucketEncryptionRequest from required parameters
newDeleteBucketEncryptionRequest :: BucketName -> DeleteBucketEncryptionRequest
newDeleteBucketEncryptionRequest _Bucket = DeleteBucketEncryptionRequest { "Bucket": _Bucket }

-- | Constructs DeleteBucketEncryptionRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newDeleteBucketEncryptionRequest' :: BucketName -> ( { "Bucket" :: (BucketName) } -> {"Bucket" :: (BucketName) } ) -> DeleteBucketEncryptionRequest
newDeleteBucketEncryptionRequest' _Bucket customize = (DeleteBucketEncryptionRequest <<< customize) { "Bucket": _Bucket }



newtype DeleteBucketInventoryConfigurationRequest = DeleteBucketInventoryConfigurationRequest 
  { "Bucket" :: (BucketName)
  , "Id" :: (InventoryId)
  }
derive instance newtypeDeleteBucketInventoryConfigurationRequest :: Newtype DeleteBucketInventoryConfigurationRequest _
derive instance repGenericDeleteBucketInventoryConfigurationRequest :: Generic DeleteBucketInventoryConfigurationRequest _
instance showDeleteBucketInventoryConfigurationRequest :: Show DeleteBucketInventoryConfigurationRequest where show = genericShow
instance decodeDeleteBucketInventoryConfigurationRequest :: Decode DeleteBucketInventoryConfigurationRequest where decode = genericDecode options
instance encodeDeleteBucketInventoryConfigurationRequest :: Encode DeleteBucketInventoryConfigurationRequest where encode = genericEncode options

-- | Constructs DeleteBucketInventoryConfigurationRequest from required parameters
newDeleteBucketInventoryConfigurationRequest :: BucketName -> InventoryId -> DeleteBucketInventoryConfigurationRequest
newDeleteBucketInventoryConfigurationRequest _Bucket _Id = DeleteBucketInventoryConfigurationRequest { "Bucket": _Bucket, "Id": _Id }

-- | Constructs DeleteBucketInventoryConfigurationRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newDeleteBucketInventoryConfigurationRequest' :: BucketName -> InventoryId -> ( { "Bucket" :: (BucketName) , "Id" :: (InventoryId) } -> {"Bucket" :: (BucketName) , "Id" :: (InventoryId) } ) -> DeleteBucketInventoryConfigurationRequest
newDeleteBucketInventoryConfigurationRequest' _Bucket _Id customize = (DeleteBucketInventoryConfigurationRequest <<< customize) { "Bucket": _Bucket, "Id": _Id }



newtype DeleteBucketLifecycleRequest = DeleteBucketLifecycleRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeDeleteBucketLifecycleRequest :: Newtype DeleteBucketLifecycleRequest _
derive instance repGenericDeleteBucketLifecycleRequest :: Generic DeleteBucketLifecycleRequest _
instance showDeleteBucketLifecycleRequest :: Show DeleteBucketLifecycleRequest where show = genericShow
instance decodeDeleteBucketLifecycleRequest :: Decode DeleteBucketLifecycleRequest where decode = genericDecode options
instance encodeDeleteBucketLifecycleRequest :: Encode DeleteBucketLifecycleRequest where encode = genericEncode options

-- | Constructs DeleteBucketLifecycleRequest from required parameters
newDeleteBucketLifecycleRequest :: BucketName -> DeleteBucketLifecycleRequest
newDeleteBucketLifecycleRequest _Bucket = DeleteBucketLifecycleRequest { "Bucket": _Bucket }

-- | Constructs DeleteBucketLifecycleRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newDeleteBucketLifecycleRequest' :: BucketName -> ( { "Bucket" :: (BucketName) } -> {"Bucket" :: (BucketName) } ) -> DeleteBucketLifecycleRequest
newDeleteBucketLifecycleRequest' _Bucket customize = (DeleteBucketLifecycleRequest <<< customize) { "Bucket": _Bucket }



newtype DeleteBucketMetricsConfigurationRequest = DeleteBucketMetricsConfigurationRequest 
  { "Bucket" :: (BucketName)
  , "Id" :: (MetricsId)
  }
derive instance newtypeDeleteBucketMetricsConfigurationRequest :: Newtype DeleteBucketMetricsConfigurationRequest _
derive instance repGenericDeleteBucketMetricsConfigurationRequest :: Generic DeleteBucketMetricsConfigurationRequest _
instance showDeleteBucketMetricsConfigurationRequest :: Show DeleteBucketMetricsConfigurationRequest where show = genericShow
instance decodeDeleteBucketMetricsConfigurationRequest :: Decode DeleteBucketMetricsConfigurationRequest where decode = genericDecode options
instance encodeDeleteBucketMetricsConfigurationRequest :: Encode DeleteBucketMetricsConfigurationRequest where encode = genericEncode options

-- | Constructs DeleteBucketMetricsConfigurationRequest from required parameters
newDeleteBucketMetricsConfigurationRequest :: BucketName -> MetricsId -> DeleteBucketMetricsConfigurationRequest
newDeleteBucketMetricsConfigurationRequest _Bucket _Id = DeleteBucketMetricsConfigurationRequest { "Bucket": _Bucket, "Id": _Id }

-- | Constructs DeleteBucketMetricsConfigurationRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newDeleteBucketMetricsConfigurationRequest' :: BucketName -> MetricsId -> ( { "Bucket" :: (BucketName) , "Id" :: (MetricsId) } -> {"Bucket" :: (BucketName) , "Id" :: (MetricsId) } ) -> DeleteBucketMetricsConfigurationRequest
newDeleteBucketMetricsConfigurationRequest' _Bucket _Id customize = (DeleteBucketMetricsConfigurationRequest <<< customize) { "Bucket": _Bucket, "Id": _Id }



newtype DeleteBucketPolicyRequest = DeleteBucketPolicyRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeDeleteBucketPolicyRequest :: Newtype DeleteBucketPolicyRequest _
derive instance repGenericDeleteBucketPolicyRequest :: Generic DeleteBucketPolicyRequest _
instance showDeleteBucketPolicyRequest :: Show DeleteBucketPolicyRequest where show = genericShow
instance decodeDeleteBucketPolicyRequest :: Decode DeleteBucketPolicyRequest where decode = genericDecode options
instance encodeDeleteBucketPolicyRequest :: Encode DeleteBucketPolicyRequest where encode = genericEncode options

-- | Constructs DeleteBucketPolicyRequest from required parameters
newDeleteBucketPolicyRequest :: BucketName -> DeleteBucketPolicyRequest
newDeleteBucketPolicyRequest _Bucket = DeleteBucketPolicyRequest { "Bucket": _Bucket }

-- | Constructs DeleteBucketPolicyRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newDeleteBucketPolicyRequest' :: BucketName -> ( { "Bucket" :: (BucketName) } -> {"Bucket" :: (BucketName) } ) -> DeleteBucketPolicyRequest
newDeleteBucketPolicyRequest' _Bucket customize = (DeleteBucketPolicyRequest <<< customize) { "Bucket": _Bucket }



newtype DeleteBucketReplicationRequest = DeleteBucketReplicationRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeDeleteBucketReplicationRequest :: Newtype DeleteBucketReplicationRequest _
derive instance repGenericDeleteBucketReplicationRequest :: Generic DeleteBucketReplicationRequest _
instance showDeleteBucketReplicationRequest :: Show DeleteBucketReplicationRequest where show = genericShow
instance decodeDeleteBucketReplicationRequest :: Decode DeleteBucketReplicationRequest where decode = genericDecode options
instance encodeDeleteBucketReplicationRequest :: Encode DeleteBucketReplicationRequest where encode = genericEncode options

-- | Constructs DeleteBucketReplicationRequest from required parameters
newDeleteBucketReplicationRequest :: BucketName -> DeleteBucketReplicationRequest
newDeleteBucketReplicationRequest _Bucket = DeleteBucketReplicationRequest { "Bucket": _Bucket }

-- | Constructs DeleteBucketReplicationRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newDeleteBucketReplicationRequest' :: BucketName -> ( { "Bucket" :: (BucketName) } -> {"Bucket" :: (BucketName) } ) -> DeleteBucketReplicationRequest
newDeleteBucketReplicationRequest' _Bucket customize = (DeleteBucketReplicationRequest <<< customize) { "Bucket": _Bucket }



newtype DeleteBucketRequest = DeleteBucketRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeDeleteBucketRequest :: Newtype DeleteBucketRequest _
derive instance repGenericDeleteBucketRequest :: Generic DeleteBucketRequest _
instance showDeleteBucketRequest :: Show DeleteBucketRequest where show = genericShow
instance decodeDeleteBucketRequest :: Decode DeleteBucketRequest where decode = genericDecode options
instance encodeDeleteBucketRequest :: Encode DeleteBucketRequest where encode = genericEncode options

-- | Constructs DeleteBucketRequest from required parameters
newDeleteBucketRequest :: BucketName -> DeleteBucketRequest
newDeleteBucketRequest _Bucket = DeleteBucketRequest { "Bucket": _Bucket }

-- | Constructs DeleteBucketRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newDeleteBucketRequest' :: BucketName -> ( { "Bucket" :: (BucketName) } -> {"Bucket" :: (BucketName) } ) -> DeleteBucketRequest
newDeleteBucketRequest' _Bucket customize = (DeleteBucketRequest <<< customize) { "Bucket": _Bucket }



newtype DeleteBucketTaggingRequest = DeleteBucketTaggingRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeDeleteBucketTaggingRequest :: Newtype DeleteBucketTaggingRequest _
derive instance repGenericDeleteBucketTaggingRequest :: Generic DeleteBucketTaggingRequest _
instance showDeleteBucketTaggingRequest :: Show DeleteBucketTaggingRequest where show = genericShow
instance decodeDeleteBucketTaggingRequest :: Decode DeleteBucketTaggingRequest where decode = genericDecode options
instance encodeDeleteBucketTaggingRequest :: Encode DeleteBucketTaggingRequest where encode = genericEncode options

-- | Constructs DeleteBucketTaggingRequest from required parameters
newDeleteBucketTaggingRequest :: BucketName -> DeleteBucketTaggingRequest
newDeleteBucketTaggingRequest _Bucket = DeleteBucketTaggingRequest { "Bucket": _Bucket }

-- | Constructs DeleteBucketTaggingRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newDeleteBucketTaggingRequest' :: BucketName -> ( { "Bucket" :: (BucketName) } -> {"Bucket" :: (BucketName) } ) -> DeleteBucketTaggingRequest
newDeleteBucketTaggingRequest' _Bucket customize = (DeleteBucketTaggingRequest <<< customize) { "Bucket": _Bucket }



newtype DeleteBucketWebsiteRequest = DeleteBucketWebsiteRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeDeleteBucketWebsiteRequest :: Newtype DeleteBucketWebsiteRequest _
derive instance repGenericDeleteBucketWebsiteRequest :: Generic DeleteBucketWebsiteRequest _
instance showDeleteBucketWebsiteRequest :: Show DeleteBucketWebsiteRequest where show = genericShow
instance decodeDeleteBucketWebsiteRequest :: Decode DeleteBucketWebsiteRequest where decode = genericDecode options
instance encodeDeleteBucketWebsiteRequest :: Encode DeleteBucketWebsiteRequest where encode = genericEncode options

-- | Constructs DeleteBucketWebsiteRequest from required parameters
newDeleteBucketWebsiteRequest :: BucketName -> DeleteBucketWebsiteRequest
newDeleteBucketWebsiteRequest _Bucket = DeleteBucketWebsiteRequest { "Bucket": _Bucket }

-- | Constructs DeleteBucketWebsiteRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newDeleteBucketWebsiteRequest' :: BucketName -> ( { "Bucket" :: (BucketName) } -> {"Bucket" :: (BucketName) } ) -> DeleteBucketWebsiteRequest
newDeleteBucketWebsiteRequest' _Bucket customize = (DeleteBucketWebsiteRequest <<< customize) { "Bucket": _Bucket }



newtype DeleteMarker = DeleteMarker Boolean
derive instance newtypeDeleteMarker :: Newtype DeleteMarker _
derive instance repGenericDeleteMarker :: Generic DeleteMarker _
instance showDeleteMarker :: Show DeleteMarker where show = genericShow
instance decodeDeleteMarker :: Decode DeleteMarker where decode = genericDecode options
instance encodeDeleteMarker :: Encode DeleteMarker where encode = genericEncode options



newtype DeleteMarkerEntry = DeleteMarkerEntry 
  { "Owner" :: NullOrUndefined (Owner)
  , "Key" :: NullOrUndefined (ObjectKey)
  , "VersionId" :: NullOrUndefined (ObjectVersionId)
  , "IsLatest" :: NullOrUndefined (IsLatest)
  , "LastModified" :: NullOrUndefined (LastModified)
  }
derive instance newtypeDeleteMarkerEntry :: Newtype DeleteMarkerEntry _
derive instance repGenericDeleteMarkerEntry :: Generic DeleteMarkerEntry _
instance showDeleteMarkerEntry :: Show DeleteMarkerEntry where show = genericShow
instance decodeDeleteMarkerEntry :: Decode DeleteMarkerEntry where decode = genericDecode options
instance encodeDeleteMarkerEntry :: Encode DeleteMarkerEntry where encode = genericEncode options

-- | Constructs DeleteMarkerEntry from required parameters
newDeleteMarkerEntry :: DeleteMarkerEntry
newDeleteMarkerEntry  = DeleteMarkerEntry { "IsLatest": (NullOrUndefined Nothing), "Key": (NullOrUndefined Nothing), "LastModified": (NullOrUndefined Nothing), "Owner": (NullOrUndefined Nothing), "VersionId": (NullOrUndefined Nothing) }

-- | Constructs DeleteMarkerEntry's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newDeleteMarkerEntry' :: ( { "Owner" :: NullOrUndefined (Owner) , "Key" :: NullOrUndefined (ObjectKey) , "VersionId" :: NullOrUndefined (ObjectVersionId) , "IsLatest" :: NullOrUndefined (IsLatest) , "LastModified" :: NullOrUndefined (LastModified) } -> {"Owner" :: NullOrUndefined (Owner) , "Key" :: NullOrUndefined (ObjectKey) , "VersionId" :: NullOrUndefined (ObjectVersionId) , "IsLatest" :: NullOrUndefined (IsLatest) , "LastModified" :: NullOrUndefined (LastModified) } ) -> DeleteMarkerEntry
newDeleteMarkerEntry'  customize = (DeleteMarkerEntry <<< customize) { "IsLatest": (NullOrUndefined Nothing), "Key": (NullOrUndefined Nothing), "LastModified": (NullOrUndefined Nothing), "Owner": (NullOrUndefined Nothing), "VersionId": (NullOrUndefined Nothing) }



newtype DeleteMarkerVersionId = DeleteMarkerVersionId String
derive instance newtypeDeleteMarkerVersionId :: Newtype DeleteMarkerVersionId _
derive instance repGenericDeleteMarkerVersionId :: Generic DeleteMarkerVersionId _
instance showDeleteMarkerVersionId :: Show DeleteMarkerVersionId where show = genericShow
instance decodeDeleteMarkerVersionId :: Decode DeleteMarkerVersionId where decode = genericDecode options
instance encodeDeleteMarkerVersionId :: Encode DeleteMarkerVersionId where encode = genericEncode options



newtype DeleteMarkers = DeleteMarkers (Array DeleteMarkerEntry)
derive instance newtypeDeleteMarkers :: Newtype DeleteMarkers _
derive instance repGenericDeleteMarkers :: Generic DeleteMarkers _
instance showDeleteMarkers :: Show DeleteMarkers where show = genericShow
instance decodeDeleteMarkers :: Decode DeleteMarkers where decode = genericDecode options
instance encodeDeleteMarkers :: Encode DeleteMarkers where encode = genericEncode options



newtype DeleteObjectOutput = DeleteObjectOutput 
  { "DeleteMarker" :: NullOrUndefined (DeleteMarker)
  , "VersionId" :: NullOrUndefined (ObjectVersionId)
  , "RequestCharged" :: NullOrUndefined (RequestCharged)
  }
derive instance newtypeDeleteObjectOutput :: Newtype DeleteObjectOutput _
derive instance repGenericDeleteObjectOutput :: Generic DeleteObjectOutput _
instance showDeleteObjectOutput :: Show DeleteObjectOutput where show = genericShow
instance decodeDeleteObjectOutput :: Decode DeleteObjectOutput where decode = genericDecode options
instance encodeDeleteObjectOutput :: Encode DeleteObjectOutput where encode = genericEncode options

-- | Constructs DeleteObjectOutput from required parameters
newDeleteObjectOutput :: DeleteObjectOutput
newDeleteObjectOutput  = DeleteObjectOutput { "DeleteMarker": (NullOrUndefined Nothing), "RequestCharged": (NullOrUndefined Nothing), "VersionId": (NullOrUndefined Nothing) }

-- | Constructs DeleteObjectOutput's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newDeleteObjectOutput' :: ( { "DeleteMarker" :: NullOrUndefined (DeleteMarker) , "VersionId" :: NullOrUndefined (ObjectVersionId) , "RequestCharged" :: NullOrUndefined (RequestCharged) } -> {"DeleteMarker" :: NullOrUndefined (DeleteMarker) , "VersionId" :: NullOrUndefined (ObjectVersionId) , "RequestCharged" :: NullOrUndefined (RequestCharged) } ) -> DeleteObjectOutput
newDeleteObjectOutput'  customize = (DeleteObjectOutput <<< customize) { "DeleteMarker": (NullOrUndefined Nothing), "RequestCharged": (NullOrUndefined Nothing), "VersionId": (NullOrUndefined Nothing) }



newtype DeleteObjectRequest = DeleteObjectRequest 
  { "Bucket" :: (BucketName)
  , "Key" :: (ObjectKey)
  , "MFA" :: NullOrUndefined (MFA)
  , "VersionId" :: NullOrUndefined (ObjectVersionId)
  , "RequestPayer" :: NullOrUndefined (RequestPayer)
  }
derive instance newtypeDeleteObjectRequest :: Newtype DeleteObjectRequest _
derive instance repGenericDeleteObjectRequest :: Generic DeleteObjectRequest _
instance showDeleteObjectRequest :: Show DeleteObjectRequest where show = genericShow
instance decodeDeleteObjectRequest :: Decode DeleteObjectRequest where decode = genericDecode options
instance encodeDeleteObjectRequest :: Encode DeleteObjectRequest where encode = genericEncode options

-- | Constructs DeleteObjectRequest from required parameters
newDeleteObjectRequest :: BucketName -> ObjectKey -> DeleteObjectRequest
newDeleteObjectRequest _Bucket _Key = DeleteObjectRequest { "Bucket": _Bucket, "Key": _Key, "MFA": (NullOrUndefined Nothing), "RequestPayer": (NullOrUndefined Nothing), "VersionId": (NullOrUndefined Nothing) }

-- | Constructs DeleteObjectRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newDeleteObjectRequest' :: BucketName -> ObjectKey -> ( { "Bucket" :: (BucketName) , "Key" :: (ObjectKey) , "MFA" :: NullOrUndefined (MFA) , "VersionId" :: NullOrUndefined (ObjectVersionId) , "RequestPayer" :: NullOrUndefined (RequestPayer) } -> {"Bucket" :: (BucketName) , "Key" :: (ObjectKey) , "MFA" :: NullOrUndefined (MFA) , "VersionId" :: NullOrUndefined (ObjectVersionId) , "RequestPayer" :: NullOrUndefined (RequestPayer) } ) -> DeleteObjectRequest
newDeleteObjectRequest' _Bucket _Key customize = (DeleteObjectRequest <<< customize) { "Bucket": _Bucket, "Key": _Key, "MFA": (NullOrUndefined Nothing), "RequestPayer": (NullOrUndefined Nothing), "VersionId": (NullOrUndefined Nothing) }



newtype DeleteObjectTaggingOutput = DeleteObjectTaggingOutput 
  { "VersionId" :: NullOrUndefined (ObjectVersionId)
  }
derive instance newtypeDeleteObjectTaggingOutput :: Newtype DeleteObjectTaggingOutput _
derive instance repGenericDeleteObjectTaggingOutput :: Generic DeleteObjectTaggingOutput _
instance showDeleteObjectTaggingOutput :: Show DeleteObjectTaggingOutput where show = genericShow
instance decodeDeleteObjectTaggingOutput :: Decode DeleteObjectTaggingOutput where decode = genericDecode options
instance encodeDeleteObjectTaggingOutput :: Encode DeleteObjectTaggingOutput where encode = genericEncode options

-- | Constructs DeleteObjectTaggingOutput from required parameters
newDeleteObjectTaggingOutput :: DeleteObjectTaggingOutput
newDeleteObjectTaggingOutput  = DeleteObjectTaggingOutput { "VersionId": (NullOrUndefined Nothing) }

-- | Constructs DeleteObjectTaggingOutput's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newDeleteObjectTaggingOutput' :: ( { "VersionId" :: NullOrUndefined (ObjectVersionId) } -> {"VersionId" :: NullOrUndefined (ObjectVersionId) } ) -> DeleteObjectTaggingOutput
newDeleteObjectTaggingOutput'  customize = (DeleteObjectTaggingOutput <<< customize) { "VersionId": (NullOrUndefined Nothing) }



newtype DeleteObjectTaggingRequest = DeleteObjectTaggingRequest 
  { "Bucket" :: (BucketName)
  , "Key" :: (ObjectKey)
  , "VersionId" :: NullOrUndefined (ObjectVersionId)
  }
derive instance newtypeDeleteObjectTaggingRequest :: Newtype DeleteObjectTaggingRequest _
derive instance repGenericDeleteObjectTaggingRequest :: Generic DeleteObjectTaggingRequest _
instance showDeleteObjectTaggingRequest :: Show DeleteObjectTaggingRequest where show = genericShow
instance decodeDeleteObjectTaggingRequest :: Decode DeleteObjectTaggingRequest where decode = genericDecode options
instance encodeDeleteObjectTaggingRequest :: Encode DeleteObjectTaggingRequest where encode = genericEncode options

-- | Constructs DeleteObjectTaggingRequest from required parameters
newDeleteObjectTaggingRequest :: BucketName -> ObjectKey -> DeleteObjectTaggingRequest
newDeleteObjectTaggingRequest _Bucket _Key = DeleteObjectTaggingRequest { "Bucket": _Bucket, "Key": _Key, "VersionId": (NullOrUndefined Nothing) }

-- | Constructs DeleteObjectTaggingRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newDeleteObjectTaggingRequest' :: BucketName -> ObjectKey -> ( { "Bucket" :: (BucketName) , "Key" :: (ObjectKey) , "VersionId" :: NullOrUndefined (ObjectVersionId) } -> {"Bucket" :: (BucketName) , "Key" :: (ObjectKey) , "VersionId" :: NullOrUndefined (ObjectVersionId) } ) -> DeleteObjectTaggingRequest
newDeleteObjectTaggingRequest' _Bucket _Key customize = (DeleteObjectTaggingRequest <<< customize) { "Bucket": _Bucket, "Key": _Key, "VersionId": (NullOrUndefined Nothing) }



newtype DeleteObjectsOutput = DeleteObjectsOutput 
  { "Deleted" :: NullOrUndefined (DeletedObjects)
  , "RequestCharged" :: NullOrUndefined (RequestCharged)
  , "Errors" :: NullOrUndefined (Errors)
  }
derive instance newtypeDeleteObjectsOutput :: Newtype DeleteObjectsOutput _
derive instance repGenericDeleteObjectsOutput :: Generic DeleteObjectsOutput _
instance showDeleteObjectsOutput :: Show DeleteObjectsOutput where show = genericShow
instance decodeDeleteObjectsOutput :: Decode DeleteObjectsOutput where decode = genericDecode options
instance encodeDeleteObjectsOutput :: Encode DeleteObjectsOutput where encode = genericEncode options

-- | Constructs DeleteObjectsOutput from required parameters
newDeleteObjectsOutput :: DeleteObjectsOutput
newDeleteObjectsOutput  = DeleteObjectsOutput { "Deleted": (NullOrUndefined Nothing), "Errors": (NullOrUndefined Nothing), "RequestCharged": (NullOrUndefined Nothing) }

-- | Constructs DeleteObjectsOutput's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newDeleteObjectsOutput' :: ( { "Deleted" :: NullOrUndefined (DeletedObjects) , "RequestCharged" :: NullOrUndefined (RequestCharged) , "Errors" :: NullOrUndefined (Errors) } -> {"Deleted" :: NullOrUndefined (DeletedObjects) , "RequestCharged" :: NullOrUndefined (RequestCharged) , "Errors" :: NullOrUndefined (Errors) } ) -> DeleteObjectsOutput
newDeleteObjectsOutput'  customize = (DeleteObjectsOutput <<< customize) { "Deleted": (NullOrUndefined Nothing), "Errors": (NullOrUndefined Nothing), "RequestCharged": (NullOrUndefined Nothing) }



newtype DeleteObjectsRequest = DeleteObjectsRequest 
  { "Bucket" :: (BucketName)
  , "Delete" :: (Delete)
  , "MFA" :: NullOrUndefined (MFA)
  , "RequestPayer" :: NullOrUndefined (RequestPayer)
  }
derive instance newtypeDeleteObjectsRequest :: Newtype DeleteObjectsRequest _
derive instance repGenericDeleteObjectsRequest :: Generic DeleteObjectsRequest _
instance showDeleteObjectsRequest :: Show DeleteObjectsRequest where show = genericShow
instance decodeDeleteObjectsRequest :: Decode DeleteObjectsRequest where decode = genericDecode options
instance encodeDeleteObjectsRequest :: Encode DeleteObjectsRequest where encode = genericEncode options

-- | Constructs DeleteObjectsRequest from required parameters
newDeleteObjectsRequest :: BucketName -> Delete -> DeleteObjectsRequest
newDeleteObjectsRequest _Bucket _Delete = DeleteObjectsRequest { "Bucket": _Bucket, "Delete": _Delete, "MFA": (NullOrUndefined Nothing), "RequestPayer": (NullOrUndefined Nothing) }

-- | Constructs DeleteObjectsRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newDeleteObjectsRequest' :: BucketName -> Delete -> ( { "Bucket" :: (BucketName) , "Delete" :: (Delete) , "MFA" :: NullOrUndefined (MFA) , "RequestPayer" :: NullOrUndefined (RequestPayer) } -> {"Bucket" :: (BucketName) , "Delete" :: (Delete) , "MFA" :: NullOrUndefined (MFA) , "RequestPayer" :: NullOrUndefined (RequestPayer) } ) -> DeleteObjectsRequest
newDeleteObjectsRequest' _Bucket _Delete customize = (DeleteObjectsRequest <<< customize) { "Bucket": _Bucket, "Delete": _Delete, "MFA": (NullOrUndefined Nothing), "RequestPayer": (NullOrUndefined Nothing) }



newtype DeletedObject = DeletedObject 
  { "Key" :: NullOrUndefined (ObjectKey)
  , "VersionId" :: NullOrUndefined (ObjectVersionId)
  , "DeleteMarker" :: NullOrUndefined (DeleteMarker)
  , "DeleteMarkerVersionId" :: NullOrUndefined (DeleteMarkerVersionId)
  }
derive instance newtypeDeletedObject :: Newtype DeletedObject _
derive instance repGenericDeletedObject :: Generic DeletedObject _
instance showDeletedObject :: Show DeletedObject where show = genericShow
instance decodeDeletedObject :: Decode DeletedObject where decode = genericDecode options
instance encodeDeletedObject :: Encode DeletedObject where encode = genericEncode options

-- | Constructs DeletedObject from required parameters
newDeletedObject :: DeletedObject
newDeletedObject  = DeletedObject { "DeleteMarker": (NullOrUndefined Nothing), "DeleteMarkerVersionId": (NullOrUndefined Nothing), "Key": (NullOrUndefined Nothing), "VersionId": (NullOrUndefined Nothing) }

-- | Constructs DeletedObject's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newDeletedObject' :: ( { "Key" :: NullOrUndefined (ObjectKey) , "VersionId" :: NullOrUndefined (ObjectVersionId) , "DeleteMarker" :: NullOrUndefined (DeleteMarker) , "DeleteMarkerVersionId" :: NullOrUndefined (DeleteMarkerVersionId) } -> {"Key" :: NullOrUndefined (ObjectKey) , "VersionId" :: NullOrUndefined (ObjectVersionId) , "DeleteMarker" :: NullOrUndefined (DeleteMarker) , "DeleteMarkerVersionId" :: NullOrUndefined (DeleteMarkerVersionId) } ) -> DeletedObject
newDeletedObject'  customize = (DeletedObject <<< customize) { "DeleteMarker": (NullOrUndefined Nothing), "DeleteMarkerVersionId": (NullOrUndefined Nothing), "Key": (NullOrUndefined Nothing), "VersionId": (NullOrUndefined Nothing) }



newtype DeletedObjects = DeletedObjects (Array DeletedObject)
derive instance newtypeDeletedObjects :: Newtype DeletedObjects _
derive instance repGenericDeletedObjects :: Generic DeletedObjects _
instance showDeletedObjects :: Show DeletedObjects where show = genericShow
instance decodeDeletedObjects :: Decode DeletedObjects where decode = genericDecode options
instance encodeDeletedObjects :: Encode DeletedObjects where encode = genericEncode options



newtype Delimiter = Delimiter String
derive instance newtypeDelimiter :: Newtype Delimiter _
derive instance repGenericDelimiter :: Generic Delimiter _
instance showDelimiter :: Show Delimiter where show = genericShow
instance decodeDelimiter :: Decode Delimiter where decode = genericDecode options
instance encodeDelimiter :: Encode Delimiter where encode = genericEncode options



newtype Description = Description String
derive instance newtypeDescription :: Newtype Description _
derive instance repGenericDescription :: Generic Description _
instance showDescription :: Show Description where show = genericShow
instance decodeDescription :: Decode Description where decode = genericDecode options
instance encodeDescription :: Encode Description where encode = genericEncode options



-- | Container for replication destination information.
newtype Destination = Destination 
  { "Bucket" :: (BucketName)
  , "Account" :: NullOrUndefined (AccountId)
  , "StorageClass" :: NullOrUndefined (StorageClass)
  , "AccessControlTranslation" :: NullOrUndefined (AccessControlTranslation)
  , "EncryptionConfiguration" :: NullOrUndefined (EncryptionConfiguration)
  }
derive instance newtypeDestination :: Newtype Destination _
derive instance repGenericDestination :: Generic Destination _
instance showDestination :: Show Destination where show = genericShow
instance decodeDestination :: Decode Destination where decode = genericDecode options
instance encodeDestination :: Encode Destination where encode = genericEncode options

-- | Constructs Destination from required parameters
newDestination :: BucketName -> Destination
newDestination _Bucket = Destination { "Bucket": _Bucket, "AccessControlTranslation": (NullOrUndefined Nothing), "Account": (NullOrUndefined Nothing), "EncryptionConfiguration": (NullOrUndefined Nothing), "StorageClass": (NullOrUndefined Nothing) }

-- | Constructs Destination's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newDestination' :: BucketName -> ( { "Bucket" :: (BucketName) , "Account" :: NullOrUndefined (AccountId) , "StorageClass" :: NullOrUndefined (StorageClass) , "AccessControlTranslation" :: NullOrUndefined (AccessControlTranslation) , "EncryptionConfiguration" :: NullOrUndefined (EncryptionConfiguration) } -> {"Bucket" :: (BucketName) , "Account" :: NullOrUndefined (AccountId) , "StorageClass" :: NullOrUndefined (StorageClass) , "AccessControlTranslation" :: NullOrUndefined (AccessControlTranslation) , "EncryptionConfiguration" :: NullOrUndefined (EncryptionConfiguration) } ) -> Destination
newDestination' _Bucket customize = (Destination <<< customize) { "Bucket": _Bucket, "AccessControlTranslation": (NullOrUndefined Nothing), "Account": (NullOrUndefined Nothing), "EncryptionConfiguration": (NullOrUndefined Nothing), "StorageClass": (NullOrUndefined Nothing) }



newtype DisplayName = DisplayName String
derive instance newtypeDisplayName :: Newtype DisplayName _
derive instance repGenericDisplayName :: Generic DisplayName _
instance showDisplayName :: Show DisplayName where show = genericShow
instance decodeDisplayName :: Decode DisplayName where decode = genericDecode options
instance encodeDisplayName :: Encode DisplayName where encode = genericEncode options



newtype ETag = ETag String
derive instance newtypeETag :: Newtype ETag _
derive instance repGenericETag :: Generic ETag _
instance showETag :: Show ETag where show = genericShow
instance decodeETag :: Decode ETag where decode = genericDecode options
instance encodeETag :: Encode ETag where encode = genericEncode options



newtype EmailAddress = EmailAddress String
derive instance newtypeEmailAddress :: Newtype EmailAddress _
derive instance repGenericEmailAddress :: Generic EmailAddress _
instance showEmailAddress :: Show EmailAddress where show = genericShow
instance decodeEmailAddress :: Decode EmailAddress where decode = genericDecode options
instance encodeEmailAddress :: Encode EmailAddress where encode = genericEncode options



-- | Requests Amazon S3 to encode the object keys in the response and specifies the encoding method to use. An object key may contain any Unicode character; however, XML 1.0 parser cannot parse some characters, such as characters with an ASCII value from 0 to 10. For characters that are not supported in XML 1.0, you can add this parameter to request that Amazon S3 encode the keys in the response.
newtype EncodingType = EncodingType String
derive instance newtypeEncodingType :: Newtype EncodingType _
derive instance repGenericEncodingType :: Generic EncodingType _
instance showEncodingType :: Show EncodingType where show = genericShow
instance decodeEncodingType :: Decode EncodingType where decode = genericDecode options
instance encodeEncodingType :: Encode EncodingType where encode = genericEncode options



-- | Describes the server-side encryption that will be applied to the restore results.
newtype Encryption = Encryption 
  { "EncryptionType" :: (ServerSideEncryption)
  , "KMSKeyId" :: NullOrUndefined (SSEKMSKeyId)
  , "KMSContext" :: NullOrUndefined (KMSContext)
  }
derive instance newtypeEncryption :: Newtype Encryption _
derive instance repGenericEncryption :: Generic Encryption _
instance showEncryption :: Show Encryption where show = genericShow
instance decodeEncryption :: Decode Encryption where decode = genericDecode options
instance encodeEncryption :: Encode Encryption where encode = genericEncode options

-- | Constructs Encryption from required parameters
newEncryption :: ServerSideEncryption -> Encryption
newEncryption _EncryptionType = Encryption { "EncryptionType": _EncryptionType, "KMSContext": (NullOrUndefined Nothing), "KMSKeyId": (NullOrUndefined Nothing) }

-- | Constructs Encryption's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newEncryption' :: ServerSideEncryption -> ( { "EncryptionType" :: (ServerSideEncryption) , "KMSKeyId" :: NullOrUndefined (SSEKMSKeyId) , "KMSContext" :: NullOrUndefined (KMSContext) } -> {"EncryptionType" :: (ServerSideEncryption) , "KMSKeyId" :: NullOrUndefined (SSEKMSKeyId) , "KMSContext" :: NullOrUndefined (KMSContext) } ) -> Encryption
newEncryption' _EncryptionType customize = (Encryption <<< customize) { "EncryptionType": _EncryptionType, "KMSContext": (NullOrUndefined Nothing), "KMSKeyId": (NullOrUndefined Nothing) }



-- | Container for information regarding encryption based configuration for replicas.
newtype EncryptionConfiguration = EncryptionConfiguration 
  { "ReplicaKmsKeyID" :: NullOrUndefined (ReplicaKmsKeyID)
  }
derive instance newtypeEncryptionConfiguration :: Newtype EncryptionConfiguration _
derive instance repGenericEncryptionConfiguration :: Generic EncryptionConfiguration _
instance showEncryptionConfiguration :: Show EncryptionConfiguration where show = genericShow
instance decodeEncryptionConfiguration :: Decode EncryptionConfiguration where decode = genericDecode options
instance encodeEncryptionConfiguration :: Encode EncryptionConfiguration where encode = genericEncode options

-- | Constructs EncryptionConfiguration from required parameters
newEncryptionConfiguration :: EncryptionConfiguration
newEncryptionConfiguration  = EncryptionConfiguration { "ReplicaKmsKeyID": (NullOrUndefined Nothing) }

-- | Constructs EncryptionConfiguration's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newEncryptionConfiguration' :: ( { "ReplicaKmsKeyID" :: NullOrUndefined (ReplicaKmsKeyID) } -> {"ReplicaKmsKeyID" :: NullOrUndefined (ReplicaKmsKeyID) } ) -> EncryptionConfiguration
newEncryptionConfiguration'  customize = (EncryptionConfiguration <<< customize) { "ReplicaKmsKeyID": (NullOrUndefined Nothing) }



newtype Error = Error 
  { "Key" :: NullOrUndefined (ObjectKey)
  , "VersionId" :: NullOrUndefined (ObjectVersionId)
  , "Code" :: NullOrUndefined (Code)
  , "Message" :: NullOrUndefined (Message)
  }
derive instance newtypeError :: Newtype Error _
derive instance repGenericError :: Generic Error _
instance showError :: Show Error where show = genericShow
instance decodeError :: Decode Error where decode = genericDecode options
instance encodeError :: Encode Error where encode = genericEncode options

-- | Constructs Error from required parameters
newError :: Error
newError  = Error { "Code": (NullOrUndefined Nothing), "Key": (NullOrUndefined Nothing), "Message": (NullOrUndefined Nothing), "VersionId": (NullOrUndefined Nothing) }

-- | Constructs Error's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newError' :: ( { "Key" :: NullOrUndefined (ObjectKey) , "VersionId" :: NullOrUndefined (ObjectVersionId) , "Code" :: NullOrUndefined (Code) , "Message" :: NullOrUndefined (Message) } -> {"Key" :: NullOrUndefined (ObjectKey) , "VersionId" :: NullOrUndefined (ObjectVersionId) , "Code" :: NullOrUndefined (Code) , "Message" :: NullOrUndefined (Message) } ) -> Error
newError'  customize = (Error <<< customize) { "Code": (NullOrUndefined Nothing), "Key": (NullOrUndefined Nothing), "Message": (NullOrUndefined Nothing), "VersionId": (NullOrUndefined Nothing) }



newtype ErrorDocument = ErrorDocument 
  { "Key" :: (ObjectKey)
  }
derive instance newtypeErrorDocument :: Newtype ErrorDocument _
derive instance repGenericErrorDocument :: Generic ErrorDocument _
instance showErrorDocument :: Show ErrorDocument where show = genericShow
instance decodeErrorDocument :: Decode ErrorDocument where decode = genericDecode options
instance encodeErrorDocument :: Encode ErrorDocument where encode = genericEncode options

-- | Constructs ErrorDocument from required parameters
newErrorDocument :: ObjectKey -> ErrorDocument
newErrorDocument _Key = ErrorDocument { "Key": _Key }

-- | Constructs ErrorDocument's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newErrorDocument' :: ObjectKey -> ( { "Key" :: (ObjectKey) } -> {"Key" :: (ObjectKey) } ) -> ErrorDocument
newErrorDocument' _Key customize = (ErrorDocument <<< customize) { "Key": _Key }



newtype Errors = Errors (Array Error)
derive instance newtypeErrors :: Newtype Errors _
derive instance repGenericErrors :: Generic Errors _
instance showErrors :: Show Errors where show = genericShow
instance decodeErrors :: Decode Errors where decode = genericDecode options
instance encodeErrors :: Encode Errors where encode = genericEncode options



-- | Bucket event for which to send notifications.
newtype Event = Event String
derive instance newtypeEvent :: Newtype Event _
derive instance repGenericEvent :: Generic Event _
instance showEvent :: Show Event where show = genericShow
instance decodeEvent :: Decode Event where decode = genericDecode options
instance encodeEvent :: Encode Event where encode = genericEncode options



newtype EventList = EventList (Array Event)
derive instance newtypeEventList :: Newtype EventList _
derive instance repGenericEventList :: Generic EventList _
instance showEventList :: Show EventList where show = genericShow
instance decodeEventList :: Decode EventList where decode = genericDecode options
instance encodeEventList :: Encode EventList where encode = genericEncode options



newtype Expiration = Expiration String
derive instance newtypeExpiration :: Newtype Expiration _
derive instance repGenericExpiration :: Generic Expiration _
instance showExpiration :: Show Expiration where show = genericShow
instance decodeExpiration :: Decode Expiration where decode = genericDecode options
instance encodeExpiration :: Encode Expiration where encode = genericEncode options



newtype ExpirationStatus = ExpirationStatus String
derive instance newtypeExpirationStatus :: Newtype ExpirationStatus _
derive instance repGenericExpirationStatus :: Generic ExpirationStatus _
instance showExpirationStatus :: Show ExpirationStatus where show = genericShow
instance decodeExpirationStatus :: Decode ExpirationStatus where decode = genericDecode options
instance encodeExpirationStatus :: Encode ExpirationStatus where encode = genericEncode options



newtype ExpiredObjectDeleteMarker = ExpiredObjectDeleteMarker Boolean
derive instance newtypeExpiredObjectDeleteMarker :: Newtype ExpiredObjectDeleteMarker _
derive instance repGenericExpiredObjectDeleteMarker :: Generic ExpiredObjectDeleteMarker _
instance showExpiredObjectDeleteMarker :: Show ExpiredObjectDeleteMarker where show = genericShow
instance decodeExpiredObjectDeleteMarker :: Decode ExpiredObjectDeleteMarker where decode = genericDecode options
instance encodeExpiredObjectDeleteMarker :: Encode ExpiredObjectDeleteMarker where encode = genericEncode options



newtype Expires = Expires Types.Timestamp
derive instance newtypeExpires :: Newtype Expires _
derive instance repGenericExpires :: Generic Expires _
instance showExpires :: Show Expires where show = genericShow
instance decodeExpires :: Decode Expires where decode = genericDecode options
instance encodeExpires :: Encode Expires where encode = genericEncode options



newtype ExposeHeader = ExposeHeader String
derive instance newtypeExposeHeader :: Newtype ExposeHeader _
derive instance repGenericExposeHeader :: Generic ExposeHeader _
instance showExposeHeader :: Show ExposeHeader where show = genericShow
instance decodeExposeHeader :: Decode ExposeHeader where decode = genericDecode options
instance encodeExposeHeader :: Encode ExposeHeader where encode = genericEncode options



newtype ExposeHeaders = ExposeHeaders (Array ExposeHeader)
derive instance newtypeExposeHeaders :: Newtype ExposeHeaders _
derive instance repGenericExposeHeaders :: Generic ExposeHeaders _
instance showExposeHeaders :: Show ExposeHeaders where show = genericShow
instance decodeExposeHeaders :: Decode ExposeHeaders where decode = genericDecode options
instance encodeExposeHeaders :: Encode ExposeHeaders where encode = genericEncode options



newtype Expression = Expression String
derive instance newtypeExpression :: Newtype Expression _
derive instance repGenericExpression :: Generic Expression _
instance showExpression :: Show Expression where show = genericShow
instance decodeExpression :: Decode Expression where decode = genericDecode options
instance encodeExpression :: Encode Expression where encode = genericEncode options



newtype ExpressionType = ExpressionType String
derive instance newtypeExpressionType :: Newtype ExpressionType _
derive instance repGenericExpressionType :: Generic ExpressionType _
instance showExpressionType :: Show ExpressionType where show = genericShow
instance decodeExpressionType :: Decode ExpressionType where decode = genericDecode options
instance encodeExpressionType :: Encode ExpressionType where encode = genericEncode options



newtype FetchOwner = FetchOwner Boolean
derive instance newtypeFetchOwner :: Newtype FetchOwner _
derive instance repGenericFetchOwner :: Generic FetchOwner _
instance showFetchOwner :: Show FetchOwner where show = genericShow
instance decodeFetchOwner :: Decode FetchOwner where decode = genericDecode options
instance encodeFetchOwner :: Encode FetchOwner where encode = genericEncode options



newtype FieldDelimiter = FieldDelimiter String
derive instance newtypeFieldDelimiter :: Newtype FieldDelimiter _
derive instance repGenericFieldDelimiter :: Generic FieldDelimiter _
instance showFieldDelimiter :: Show FieldDelimiter where show = genericShow
instance decodeFieldDelimiter :: Decode FieldDelimiter where decode = genericDecode options
instance encodeFieldDelimiter :: Encode FieldDelimiter where encode = genericEncode options



newtype FileHeaderInfo = FileHeaderInfo String
derive instance newtypeFileHeaderInfo :: Newtype FileHeaderInfo _
derive instance repGenericFileHeaderInfo :: Generic FileHeaderInfo _
instance showFileHeaderInfo :: Show FileHeaderInfo where show = genericShow
instance decodeFileHeaderInfo :: Decode FileHeaderInfo where decode = genericDecode options
instance encodeFileHeaderInfo :: Encode FileHeaderInfo where encode = genericEncode options



-- | Container for key value pair that defines the criteria for the filter rule.
newtype FilterRule = FilterRule 
  { "Name" :: NullOrUndefined (FilterRuleName)
  , "Value" :: NullOrUndefined (FilterRuleValue)
  }
derive instance newtypeFilterRule :: Newtype FilterRule _
derive instance repGenericFilterRule :: Generic FilterRule _
instance showFilterRule :: Show FilterRule where show = genericShow
instance decodeFilterRule :: Decode FilterRule where decode = genericDecode options
instance encodeFilterRule :: Encode FilterRule where encode = genericEncode options

-- | Constructs FilterRule from required parameters
newFilterRule :: FilterRule
newFilterRule  = FilterRule { "Name": (NullOrUndefined Nothing), "Value": (NullOrUndefined Nothing) }

-- | Constructs FilterRule's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newFilterRule' :: ( { "Name" :: NullOrUndefined (FilterRuleName) , "Value" :: NullOrUndefined (FilterRuleValue) } -> {"Name" :: NullOrUndefined (FilterRuleName) , "Value" :: NullOrUndefined (FilterRuleValue) } ) -> FilterRule
newFilterRule'  customize = (FilterRule <<< customize) { "Name": (NullOrUndefined Nothing), "Value": (NullOrUndefined Nothing) }



-- | A list of containers for key value pair that defines the criteria for the filter rule.
newtype FilterRuleList = FilterRuleList (Array FilterRule)
derive instance newtypeFilterRuleList :: Newtype FilterRuleList _
derive instance repGenericFilterRuleList :: Generic FilterRuleList _
instance showFilterRuleList :: Show FilterRuleList where show = genericShow
instance decodeFilterRuleList :: Decode FilterRuleList where decode = genericDecode options
instance encodeFilterRuleList :: Encode FilterRuleList where encode = genericEncode options



newtype FilterRuleName = FilterRuleName String
derive instance newtypeFilterRuleName :: Newtype FilterRuleName _
derive instance repGenericFilterRuleName :: Generic FilterRuleName _
instance showFilterRuleName :: Show FilterRuleName where show = genericShow
instance decodeFilterRuleName :: Decode FilterRuleName where decode = genericDecode options
instance encodeFilterRuleName :: Encode FilterRuleName where encode = genericEncode options



newtype FilterRuleValue = FilterRuleValue String
derive instance newtypeFilterRuleValue :: Newtype FilterRuleValue _
derive instance repGenericFilterRuleValue :: Generic FilterRuleValue _
instance showFilterRuleValue :: Show FilterRuleValue where show = genericShow
instance decodeFilterRuleValue :: Decode FilterRuleValue where decode = genericDecode options
instance encodeFilterRuleValue :: Encode FilterRuleValue where encode = genericEncode options



newtype GetBucketAccelerateConfigurationOutput = GetBucketAccelerateConfigurationOutput 
  { "Status" :: NullOrUndefined (BucketAccelerateStatus)
  }
derive instance newtypeGetBucketAccelerateConfigurationOutput :: Newtype GetBucketAccelerateConfigurationOutput _
derive instance repGenericGetBucketAccelerateConfigurationOutput :: Generic GetBucketAccelerateConfigurationOutput _
instance showGetBucketAccelerateConfigurationOutput :: Show GetBucketAccelerateConfigurationOutput where show = genericShow
instance decodeGetBucketAccelerateConfigurationOutput :: Decode GetBucketAccelerateConfigurationOutput where decode = genericDecode options
instance encodeGetBucketAccelerateConfigurationOutput :: Encode GetBucketAccelerateConfigurationOutput where encode = genericEncode options

-- | Constructs GetBucketAccelerateConfigurationOutput from required parameters
newGetBucketAccelerateConfigurationOutput :: GetBucketAccelerateConfigurationOutput
newGetBucketAccelerateConfigurationOutput  = GetBucketAccelerateConfigurationOutput { "Status": (NullOrUndefined Nothing) }

-- | Constructs GetBucketAccelerateConfigurationOutput's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newGetBucketAccelerateConfigurationOutput' :: ( { "Status" :: NullOrUndefined (BucketAccelerateStatus) } -> {"Status" :: NullOrUndefined (BucketAccelerateStatus) } ) -> GetBucketAccelerateConfigurationOutput
newGetBucketAccelerateConfigurationOutput'  customize = (GetBucketAccelerateConfigurationOutput <<< customize) { "Status": (NullOrUndefined Nothing) }



newtype GetBucketAccelerateConfigurationRequest = GetBucketAccelerateConfigurationRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeGetBucketAccelerateConfigurationRequest :: Newtype GetBucketAccelerateConfigurationRequest _
derive instance repGenericGetBucketAccelerateConfigurationRequest :: Generic GetBucketAccelerateConfigurationRequest _
instance showGetBucketAccelerateConfigurationRequest :: Show GetBucketAccelerateConfigurationRequest where show = genericShow
instance decodeGetBucketAccelerateConfigurationRequest :: Decode GetBucketAccelerateConfigurationRequest where decode = genericDecode options
instance encodeGetBucketAccelerateConfigurationRequest :: Encode GetBucketAccelerateConfigurationRequest where encode = genericEncode options

-- | Constructs GetBucketAccelerateConfigurationRequest from required parameters
newGetBucketAccelerateConfigurationRequest :: BucketName -> GetBucketAccelerateConfigurationRequest
newGetBucketAccelerateConfigurationRequest _Bucket = GetBucketAccelerateConfigurationRequest { "Bucket": _Bucket }

-- | Constructs GetBucketAccelerateConfigurationRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newGetBucketAccelerateConfigurationRequest' :: BucketName -> ( { "Bucket" :: (BucketName) } -> {"Bucket" :: (BucketName) } ) -> GetBucketAccelerateConfigurationRequest
newGetBucketAccelerateConfigurationRequest' _Bucket customize = (GetBucketAccelerateConfigurationRequest <<< customize) { "Bucket": _Bucket }



newtype GetBucketAclOutput = GetBucketAclOutput 
  { "Owner" :: NullOrUndefined (Owner)
  , "Grants" :: NullOrUndefined (Grants)
  }
derive instance newtypeGetBucketAclOutput :: Newtype GetBucketAclOutput _
derive instance repGenericGetBucketAclOutput :: Generic GetBucketAclOutput _
instance showGetBucketAclOutput :: Show GetBucketAclOutput where show = genericShow
instance decodeGetBucketAclOutput :: Decode GetBucketAclOutput where decode = genericDecode options
instance encodeGetBucketAclOutput :: Encode GetBucketAclOutput where encode = genericEncode options

-- | Constructs GetBucketAclOutput from required parameters
newGetBucketAclOutput :: GetBucketAclOutput
newGetBucketAclOutput  = GetBucketAclOutput { "Grants": (NullOrUndefined Nothing), "Owner": (NullOrUndefined Nothing) }

-- | Constructs GetBucketAclOutput's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newGetBucketAclOutput' :: ( { "Owner" :: NullOrUndefined (Owner) , "Grants" :: NullOrUndefined (Grants) } -> {"Owner" :: NullOrUndefined (Owner) , "Grants" :: NullOrUndefined (Grants) } ) -> GetBucketAclOutput
newGetBucketAclOutput'  customize = (GetBucketAclOutput <<< customize) { "Grants": (NullOrUndefined Nothing), "Owner": (NullOrUndefined Nothing) }



newtype GetBucketAclRequest = GetBucketAclRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeGetBucketAclRequest :: Newtype GetBucketAclRequest _
derive instance repGenericGetBucketAclRequest :: Generic GetBucketAclRequest _
instance showGetBucketAclRequest :: Show GetBucketAclRequest where show = genericShow
instance decodeGetBucketAclRequest :: Decode GetBucketAclRequest where decode = genericDecode options
instance encodeGetBucketAclRequest :: Encode GetBucketAclRequest where encode = genericEncode options

-- | Constructs GetBucketAclRequest from required parameters
newGetBucketAclRequest :: BucketName -> GetBucketAclRequest
newGetBucketAclRequest _Bucket = GetBucketAclRequest { "Bucket": _Bucket }

-- | Constructs GetBucketAclRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newGetBucketAclRequest' :: BucketName -> ( { "Bucket" :: (BucketName) } -> {"Bucket" :: (BucketName) } ) -> GetBucketAclRequest
newGetBucketAclRequest' _Bucket customize = (GetBucketAclRequest <<< customize) { "Bucket": _Bucket }



newtype GetBucketAnalyticsConfigurationOutput = GetBucketAnalyticsConfigurationOutput 
  { "AnalyticsConfiguration" :: NullOrUndefined (AnalyticsConfiguration)
  }
derive instance newtypeGetBucketAnalyticsConfigurationOutput :: Newtype GetBucketAnalyticsConfigurationOutput _
derive instance repGenericGetBucketAnalyticsConfigurationOutput :: Generic GetBucketAnalyticsConfigurationOutput _
instance showGetBucketAnalyticsConfigurationOutput :: Show GetBucketAnalyticsConfigurationOutput where show = genericShow
instance decodeGetBucketAnalyticsConfigurationOutput :: Decode GetBucketAnalyticsConfigurationOutput where decode = genericDecode options
instance encodeGetBucketAnalyticsConfigurationOutput :: Encode GetBucketAnalyticsConfigurationOutput where encode = genericEncode options

-- | Constructs GetBucketAnalyticsConfigurationOutput from required parameters
newGetBucketAnalyticsConfigurationOutput :: GetBucketAnalyticsConfigurationOutput
newGetBucketAnalyticsConfigurationOutput  = GetBucketAnalyticsConfigurationOutput { "AnalyticsConfiguration": (NullOrUndefined Nothing) }

-- | Constructs GetBucketAnalyticsConfigurationOutput's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newGetBucketAnalyticsConfigurationOutput' :: ( { "AnalyticsConfiguration" :: NullOrUndefined (AnalyticsConfiguration) } -> {"AnalyticsConfiguration" :: NullOrUndefined (AnalyticsConfiguration) } ) -> GetBucketAnalyticsConfigurationOutput
newGetBucketAnalyticsConfigurationOutput'  customize = (GetBucketAnalyticsConfigurationOutput <<< customize) { "AnalyticsConfiguration": (NullOrUndefined Nothing) }



newtype GetBucketAnalyticsConfigurationRequest = GetBucketAnalyticsConfigurationRequest 
  { "Bucket" :: (BucketName)
  , "Id" :: (AnalyticsId)
  }
derive instance newtypeGetBucketAnalyticsConfigurationRequest :: Newtype GetBucketAnalyticsConfigurationRequest _
derive instance repGenericGetBucketAnalyticsConfigurationRequest :: Generic GetBucketAnalyticsConfigurationRequest _
instance showGetBucketAnalyticsConfigurationRequest :: Show GetBucketAnalyticsConfigurationRequest where show = genericShow
instance decodeGetBucketAnalyticsConfigurationRequest :: Decode GetBucketAnalyticsConfigurationRequest where decode = genericDecode options
instance encodeGetBucketAnalyticsConfigurationRequest :: Encode GetBucketAnalyticsConfigurationRequest where encode = genericEncode options

-- | Constructs GetBucketAnalyticsConfigurationRequest from required parameters
newGetBucketAnalyticsConfigurationRequest :: BucketName -> AnalyticsId -> GetBucketAnalyticsConfigurationRequest
newGetBucketAnalyticsConfigurationRequest _Bucket _Id = GetBucketAnalyticsConfigurationRequest { "Bucket": _Bucket, "Id": _Id }

-- | Constructs GetBucketAnalyticsConfigurationRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newGetBucketAnalyticsConfigurationRequest' :: BucketName -> AnalyticsId -> ( { "Bucket" :: (BucketName) , "Id" :: (AnalyticsId) } -> {"Bucket" :: (BucketName) , "Id" :: (AnalyticsId) } ) -> GetBucketAnalyticsConfigurationRequest
newGetBucketAnalyticsConfigurationRequest' _Bucket _Id customize = (GetBucketAnalyticsConfigurationRequest <<< customize) { "Bucket": _Bucket, "Id": _Id }



newtype GetBucketCorsOutput = GetBucketCorsOutput 
  { "CORSRules" :: NullOrUndefined (CORSRules)
  }
derive instance newtypeGetBucketCorsOutput :: Newtype GetBucketCorsOutput _
derive instance repGenericGetBucketCorsOutput :: Generic GetBucketCorsOutput _
instance showGetBucketCorsOutput :: Show GetBucketCorsOutput where show = genericShow
instance decodeGetBucketCorsOutput :: Decode GetBucketCorsOutput where decode = genericDecode options
instance encodeGetBucketCorsOutput :: Encode GetBucketCorsOutput where encode = genericEncode options

-- | Constructs GetBucketCorsOutput from required parameters
newGetBucketCorsOutput :: GetBucketCorsOutput
newGetBucketCorsOutput  = GetBucketCorsOutput { "CORSRules": (NullOrUndefined Nothing) }

-- | Constructs GetBucketCorsOutput's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newGetBucketCorsOutput' :: ( { "CORSRules" :: NullOrUndefined (CORSRules) } -> {"CORSRules" :: NullOrUndefined (CORSRules) } ) -> GetBucketCorsOutput
newGetBucketCorsOutput'  customize = (GetBucketCorsOutput <<< customize) { "CORSRules": (NullOrUndefined Nothing) }



newtype GetBucketCorsRequest = GetBucketCorsRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeGetBucketCorsRequest :: Newtype GetBucketCorsRequest _
derive instance repGenericGetBucketCorsRequest :: Generic GetBucketCorsRequest _
instance showGetBucketCorsRequest :: Show GetBucketCorsRequest where show = genericShow
instance decodeGetBucketCorsRequest :: Decode GetBucketCorsRequest where decode = genericDecode options
instance encodeGetBucketCorsRequest :: Encode GetBucketCorsRequest where encode = genericEncode options

-- | Constructs GetBucketCorsRequest from required parameters
newGetBucketCorsRequest :: BucketName -> GetBucketCorsRequest
newGetBucketCorsRequest _Bucket = GetBucketCorsRequest { "Bucket": _Bucket }

-- | Constructs GetBucketCorsRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newGetBucketCorsRequest' :: BucketName -> ( { "Bucket" :: (BucketName) } -> {"Bucket" :: (BucketName) } ) -> GetBucketCorsRequest
newGetBucketCorsRequest' _Bucket customize = (GetBucketCorsRequest <<< customize) { "Bucket": _Bucket }



newtype GetBucketEncryptionOutput = GetBucketEncryptionOutput 
  { "ServerSideEncryptionConfiguration" :: NullOrUndefined (ServerSideEncryptionConfiguration)
  }
derive instance newtypeGetBucketEncryptionOutput :: Newtype GetBucketEncryptionOutput _
derive instance repGenericGetBucketEncryptionOutput :: Generic GetBucketEncryptionOutput _
instance showGetBucketEncryptionOutput :: Show GetBucketEncryptionOutput where show = genericShow
instance decodeGetBucketEncryptionOutput :: Decode GetBucketEncryptionOutput where decode = genericDecode options
instance encodeGetBucketEncryptionOutput :: Encode GetBucketEncryptionOutput where encode = genericEncode options

-- | Constructs GetBucketEncryptionOutput from required parameters
newGetBucketEncryptionOutput :: GetBucketEncryptionOutput
newGetBucketEncryptionOutput  = GetBucketEncryptionOutput { "ServerSideEncryptionConfiguration": (NullOrUndefined Nothing) }

-- | Constructs GetBucketEncryptionOutput's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newGetBucketEncryptionOutput' :: ( { "ServerSideEncryptionConfiguration" :: NullOrUndefined (ServerSideEncryptionConfiguration) } -> {"ServerSideEncryptionConfiguration" :: NullOrUndefined (ServerSideEncryptionConfiguration) } ) -> GetBucketEncryptionOutput
newGetBucketEncryptionOutput'  customize = (GetBucketEncryptionOutput <<< customize) { "ServerSideEncryptionConfiguration": (NullOrUndefined Nothing) }



newtype GetBucketEncryptionRequest = GetBucketEncryptionRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeGetBucketEncryptionRequest :: Newtype GetBucketEncryptionRequest _
derive instance repGenericGetBucketEncryptionRequest :: Generic GetBucketEncryptionRequest _
instance showGetBucketEncryptionRequest :: Show GetBucketEncryptionRequest where show = genericShow
instance decodeGetBucketEncryptionRequest :: Decode GetBucketEncryptionRequest where decode = genericDecode options
instance encodeGetBucketEncryptionRequest :: Encode GetBucketEncryptionRequest where encode = genericEncode options

-- | Constructs GetBucketEncryptionRequest from required parameters
newGetBucketEncryptionRequest :: BucketName -> GetBucketEncryptionRequest
newGetBucketEncryptionRequest _Bucket = GetBucketEncryptionRequest { "Bucket": _Bucket }

-- | Constructs GetBucketEncryptionRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newGetBucketEncryptionRequest' :: BucketName -> ( { "Bucket" :: (BucketName) } -> {"Bucket" :: (BucketName) } ) -> GetBucketEncryptionRequest
newGetBucketEncryptionRequest' _Bucket customize = (GetBucketEncryptionRequest <<< customize) { "Bucket": _Bucket }



newtype GetBucketInventoryConfigurationOutput = GetBucketInventoryConfigurationOutput 
  { "InventoryConfiguration" :: NullOrUndefined (InventoryConfiguration)
  }
derive instance newtypeGetBucketInventoryConfigurationOutput :: Newtype GetBucketInventoryConfigurationOutput _
derive instance repGenericGetBucketInventoryConfigurationOutput :: Generic GetBucketInventoryConfigurationOutput _
instance showGetBucketInventoryConfigurationOutput :: Show GetBucketInventoryConfigurationOutput where show = genericShow
instance decodeGetBucketInventoryConfigurationOutput :: Decode GetBucketInventoryConfigurationOutput where decode = genericDecode options
instance encodeGetBucketInventoryConfigurationOutput :: Encode GetBucketInventoryConfigurationOutput where encode = genericEncode options

-- | Constructs GetBucketInventoryConfigurationOutput from required parameters
newGetBucketInventoryConfigurationOutput :: GetBucketInventoryConfigurationOutput
newGetBucketInventoryConfigurationOutput  = GetBucketInventoryConfigurationOutput { "InventoryConfiguration": (NullOrUndefined Nothing) }

-- | Constructs GetBucketInventoryConfigurationOutput's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newGetBucketInventoryConfigurationOutput' :: ( { "InventoryConfiguration" :: NullOrUndefined (InventoryConfiguration) } -> {"InventoryConfiguration" :: NullOrUndefined (InventoryConfiguration) } ) -> GetBucketInventoryConfigurationOutput
newGetBucketInventoryConfigurationOutput'  customize = (GetBucketInventoryConfigurationOutput <<< customize) { "InventoryConfiguration": (NullOrUndefined Nothing) }



newtype GetBucketInventoryConfigurationRequest = GetBucketInventoryConfigurationRequest 
  { "Bucket" :: (BucketName)
  , "Id" :: (InventoryId)
  }
derive instance newtypeGetBucketInventoryConfigurationRequest :: Newtype GetBucketInventoryConfigurationRequest _
derive instance repGenericGetBucketInventoryConfigurationRequest :: Generic GetBucketInventoryConfigurationRequest _
instance showGetBucketInventoryConfigurationRequest :: Show GetBucketInventoryConfigurationRequest where show = genericShow
instance decodeGetBucketInventoryConfigurationRequest :: Decode GetBucketInventoryConfigurationRequest where decode = genericDecode options
instance encodeGetBucketInventoryConfigurationRequest :: Encode GetBucketInventoryConfigurationRequest where encode = genericEncode options

-- | Constructs GetBucketInventoryConfigurationRequest from required parameters
newGetBucketInventoryConfigurationRequest :: BucketName -> InventoryId -> GetBucketInventoryConfigurationRequest
newGetBucketInventoryConfigurationRequest _Bucket _Id = GetBucketInventoryConfigurationRequest { "Bucket": _Bucket, "Id": _Id }

-- | Constructs GetBucketInventoryConfigurationRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newGetBucketInventoryConfigurationRequest' :: BucketName -> InventoryId -> ( { "Bucket" :: (BucketName) , "Id" :: (InventoryId) } -> {"Bucket" :: (BucketName) , "Id" :: (InventoryId) } ) -> GetBucketInventoryConfigurationRequest
newGetBucketInventoryConfigurationRequest' _Bucket _Id customize = (GetBucketInventoryConfigurationRequest <<< customize) { "Bucket": _Bucket, "Id": _Id }



newtype GetBucketLifecycleConfigurationOutput = GetBucketLifecycleConfigurationOutput 
  { "Rules" :: NullOrUndefined (LifecycleRules)
  }
derive instance newtypeGetBucketLifecycleConfigurationOutput :: Newtype GetBucketLifecycleConfigurationOutput _
derive instance repGenericGetBucketLifecycleConfigurationOutput :: Generic GetBucketLifecycleConfigurationOutput _
instance showGetBucketLifecycleConfigurationOutput :: Show GetBucketLifecycleConfigurationOutput where show = genericShow
instance decodeGetBucketLifecycleConfigurationOutput :: Decode GetBucketLifecycleConfigurationOutput where decode = genericDecode options
instance encodeGetBucketLifecycleConfigurationOutput :: Encode GetBucketLifecycleConfigurationOutput where encode = genericEncode options

-- | Constructs GetBucketLifecycleConfigurationOutput from required parameters
newGetBucketLifecycleConfigurationOutput :: GetBucketLifecycleConfigurationOutput
newGetBucketLifecycleConfigurationOutput  = GetBucketLifecycleConfigurationOutput { "Rules": (NullOrUndefined Nothing) }

-- | Constructs GetBucketLifecycleConfigurationOutput's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newGetBucketLifecycleConfigurationOutput' :: ( { "Rules" :: NullOrUndefined (LifecycleRules) } -> {"Rules" :: NullOrUndefined (LifecycleRules) } ) -> GetBucketLifecycleConfigurationOutput
newGetBucketLifecycleConfigurationOutput'  customize = (GetBucketLifecycleConfigurationOutput <<< customize) { "Rules": (NullOrUndefined Nothing) }



newtype GetBucketLifecycleConfigurationRequest = GetBucketLifecycleConfigurationRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeGetBucketLifecycleConfigurationRequest :: Newtype GetBucketLifecycleConfigurationRequest _
derive instance repGenericGetBucketLifecycleConfigurationRequest :: Generic GetBucketLifecycleConfigurationRequest _
instance showGetBucketLifecycleConfigurationRequest :: Show GetBucketLifecycleConfigurationRequest where show = genericShow
instance decodeGetBucketLifecycleConfigurationRequest :: Decode GetBucketLifecycleConfigurationRequest where decode = genericDecode options
instance encodeGetBucketLifecycleConfigurationRequest :: Encode GetBucketLifecycleConfigurationRequest where encode = genericEncode options

-- | Constructs GetBucketLifecycleConfigurationRequest from required parameters
newGetBucketLifecycleConfigurationRequest :: BucketName -> GetBucketLifecycleConfigurationRequest
newGetBucketLifecycleConfigurationRequest _Bucket = GetBucketLifecycleConfigurationRequest { "Bucket": _Bucket }

-- | Constructs GetBucketLifecycleConfigurationRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newGetBucketLifecycleConfigurationRequest' :: BucketName -> ( { "Bucket" :: (BucketName) } -> {"Bucket" :: (BucketName) } ) -> GetBucketLifecycleConfigurationRequest
newGetBucketLifecycleConfigurationRequest' _Bucket customize = (GetBucketLifecycleConfigurationRequest <<< customize) { "Bucket": _Bucket }



newtype GetBucketLifecycleOutput = GetBucketLifecycleOutput 
  { "Rules" :: NullOrUndefined (Rules)
  }
derive instance newtypeGetBucketLifecycleOutput :: Newtype GetBucketLifecycleOutput _
derive instance repGenericGetBucketLifecycleOutput :: Generic GetBucketLifecycleOutput _
instance showGetBucketLifecycleOutput :: Show GetBucketLifecycleOutput where show = genericShow
instance decodeGetBucketLifecycleOutput :: Decode GetBucketLifecycleOutput where decode = genericDecode options
instance encodeGetBucketLifecycleOutput :: Encode GetBucketLifecycleOutput where encode = genericEncode options

-- | Constructs GetBucketLifecycleOutput from required parameters
newGetBucketLifecycleOutput :: GetBucketLifecycleOutput
newGetBucketLifecycleOutput  = GetBucketLifecycleOutput { "Rules": (NullOrUndefined Nothing) }

-- | Constructs GetBucketLifecycleOutput's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newGetBucketLifecycleOutput' :: ( { "Rules" :: NullOrUndefined (Rules) } -> {"Rules" :: NullOrUndefined (Rules) } ) -> GetBucketLifecycleOutput
newGetBucketLifecycleOutput'  customize = (GetBucketLifecycleOutput <<< customize) { "Rules": (NullOrUndefined Nothing) }



newtype GetBucketLifecycleRequest = GetBucketLifecycleRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeGetBucketLifecycleRequest :: Newtype GetBucketLifecycleRequest _
derive instance repGenericGetBucketLifecycleRequest :: Generic GetBucketLifecycleRequest _
instance showGetBucketLifecycleRequest :: Show GetBucketLifecycleRequest where show = genericShow
instance decodeGetBucketLifecycleRequest :: Decode GetBucketLifecycleRequest where decode = genericDecode options
instance encodeGetBucketLifecycleRequest :: Encode GetBucketLifecycleRequest where encode = genericEncode options

-- | Constructs GetBucketLifecycleRequest from required parameters
newGetBucketLifecycleRequest :: BucketName -> GetBucketLifecycleRequest
newGetBucketLifecycleRequest _Bucket = GetBucketLifecycleRequest { "Bucket": _Bucket }

-- | Constructs GetBucketLifecycleRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newGetBucketLifecycleRequest' :: BucketName -> ( { "Bucket" :: (BucketName) } -> {"Bucket" :: (BucketName) } ) -> GetBucketLifecycleRequest
newGetBucketLifecycleRequest' _Bucket customize = (GetBucketLifecycleRequest <<< customize) { "Bucket": _Bucket }



newtype GetBucketLocationOutput = GetBucketLocationOutput 
  { "LocationConstraint" :: NullOrUndefined (BucketLocationConstraint)
  }
derive instance newtypeGetBucketLocationOutput :: Newtype GetBucketLocationOutput _
derive instance repGenericGetBucketLocationOutput :: Generic GetBucketLocationOutput _
instance showGetBucketLocationOutput :: Show GetBucketLocationOutput where show = genericShow
instance decodeGetBucketLocationOutput :: Decode GetBucketLocationOutput where decode = genericDecode options
instance encodeGetBucketLocationOutput :: Encode GetBucketLocationOutput where encode = genericEncode options

-- | Constructs GetBucketLocationOutput from required parameters
newGetBucketLocationOutput :: GetBucketLocationOutput
newGetBucketLocationOutput  = GetBucketLocationOutput { "LocationConstraint": (NullOrUndefined Nothing) }

-- | Constructs GetBucketLocationOutput's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newGetBucketLocationOutput' :: ( { "LocationConstraint" :: NullOrUndefined (BucketLocationConstraint) } -> {"LocationConstraint" :: NullOrUndefined (BucketLocationConstraint) } ) -> GetBucketLocationOutput
newGetBucketLocationOutput'  customize = (GetBucketLocationOutput <<< customize) { "LocationConstraint": (NullOrUndefined Nothing) }



newtype GetBucketLocationRequest = GetBucketLocationRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeGetBucketLocationRequest :: Newtype GetBucketLocationRequest _
derive instance repGenericGetBucketLocationRequest :: Generic GetBucketLocationRequest _
instance showGetBucketLocationRequest :: Show GetBucketLocationRequest where show = genericShow
instance decodeGetBucketLocationRequest :: Decode GetBucketLocationRequest where decode = genericDecode options
instance encodeGetBucketLocationRequest :: Encode GetBucketLocationRequest where encode = genericEncode options

-- | Constructs GetBucketLocationRequest from required parameters
newGetBucketLocationRequest :: BucketName -> GetBucketLocationRequest
newGetBucketLocationRequest _Bucket = GetBucketLocationRequest { "Bucket": _Bucket }

-- | Constructs GetBucketLocationRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newGetBucketLocationRequest' :: BucketName -> ( { "Bucket" :: (BucketName) } -> {"Bucket" :: (BucketName) } ) -> GetBucketLocationRequest
newGetBucketLocationRequest' _Bucket customize = (GetBucketLocationRequest <<< customize) { "Bucket": _Bucket }



newtype GetBucketLoggingOutput = GetBucketLoggingOutput 
  { "LoggingEnabled" :: NullOrUndefined (LoggingEnabled)
  }
derive instance newtypeGetBucketLoggingOutput :: Newtype GetBucketLoggingOutput _
derive instance repGenericGetBucketLoggingOutput :: Generic GetBucketLoggingOutput _
instance showGetBucketLoggingOutput :: Show GetBucketLoggingOutput where show = genericShow
instance decodeGetBucketLoggingOutput :: Decode GetBucketLoggingOutput where decode = genericDecode options
instance encodeGetBucketLoggingOutput :: Encode GetBucketLoggingOutput where encode = genericEncode options

-- | Constructs GetBucketLoggingOutput from required parameters
newGetBucketLoggingOutput :: GetBucketLoggingOutput
newGetBucketLoggingOutput  = GetBucketLoggingOutput { "LoggingEnabled": (NullOrUndefined Nothing) }

-- | Constructs GetBucketLoggingOutput's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newGetBucketLoggingOutput' :: ( { "LoggingEnabled" :: NullOrUndefined (LoggingEnabled) } -> {"LoggingEnabled" :: NullOrUndefined (LoggingEnabled) } ) -> GetBucketLoggingOutput
newGetBucketLoggingOutput'  customize = (GetBucketLoggingOutput <<< customize) { "LoggingEnabled": (NullOrUndefined Nothing) }



newtype GetBucketLoggingRequest = GetBucketLoggingRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeGetBucketLoggingRequest :: Newtype GetBucketLoggingRequest _
derive instance repGenericGetBucketLoggingRequest :: Generic GetBucketLoggingRequest _
instance showGetBucketLoggingRequest :: Show GetBucketLoggingRequest where show = genericShow
instance decodeGetBucketLoggingRequest :: Decode GetBucketLoggingRequest where decode = genericDecode options
instance encodeGetBucketLoggingRequest :: Encode GetBucketLoggingRequest where encode = genericEncode options

-- | Constructs GetBucketLoggingRequest from required parameters
newGetBucketLoggingRequest :: BucketName -> GetBucketLoggingRequest
newGetBucketLoggingRequest _Bucket = GetBucketLoggingRequest { "Bucket": _Bucket }

-- | Constructs GetBucketLoggingRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newGetBucketLoggingRequest' :: BucketName -> ( { "Bucket" :: (BucketName) } -> {"Bucket" :: (BucketName) } ) -> GetBucketLoggingRequest
newGetBucketLoggingRequest' _Bucket customize = (GetBucketLoggingRequest <<< customize) { "Bucket": _Bucket }



newtype GetBucketMetricsConfigurationOutput = GetBucketMetricsConfigurationOutput 
  { "MetricsConfiguration" :: NullOrUndefined (MetricsConfiguration)
  }
derive instance newtypeGetBucketMetricsConfigurationOutput :: Newtype GetBucketMetricsConfigurationOutput _
derive instance repGenericGetBucketMetricsConfigurationOutput :: Generic GetBucketMetricsConfigurationOutput _
instance showGetBucketMetricsConfigurationOutput :: Show GetBucketMetricsConfigurationOutput where show = genericShow
instance decodeGetBucketMetricsConfigurationOutput :: Decode GetBucketMetricsConfigurationOutput where decode = genericDecode options
instance encodeGetBucketMetricsConfigurationOutput :: Encode GetBucketMetricsConfigurationOutput where encode = genericEncode options

-- | Constructs GetBucketMetricsConfigurationOutput from required parameters
newGetBucketMetricsConfigurationOutput :: GetBucketMetricsConfigurationOutput
newGetBucketMetricsConfigurationOutput  = GetBucketMetricsConfigurationOutput { "MetricsConfiguration": (NullOrUndefined Nothing) }

-- | Constructs GetBucketMetricsConfigurationOutput's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newGetBucketMetricsConfigurationOutput' :: ( { "MetricsConfiguration" :: NullOrUndefined (MetricsConfiguration) } -> {"MetricsConfiguration" :: NullOrUndefined (MetricsConfiguration) } ) -> GetBucketMetricsConfigurationOutput
newGetBucketMetricsConfigurationOutput'  customize = (GetBucketMetricsConfigurationOutput <<< customize) { "MetricsConfiguration": (NullOrUndefined Nothing) }



newtype GetBucketMetricsConfigurationRequest = GetBucketMetricsConfigurationRequest 
  { "Bucket" :: (BucketName)
  , "Id" :: (MetricsId)
  }
derive instance newtypeGetBucketMetricsConfigurationRequest :: Newtype GetBucketMetricsConfigurationRequest _
derive instance repGenericGetBucketMetricsConfigurationRequest :: Generic GetBucketMetricsConfigurationRequest _
instance showGetBucketMetricsConfigurationRequest :: Show GetBucketMetricsConfigurationRequest where show = genericShow
instance decodeGetBucketMetricsConfigurationRequest :: Decode GetBucketMetricsConfigurationRequest where decode = genericDecode options
instance encodeGetBucketMetricsConfigurationRequest :: Encode GetBucketMetricsConfigurationRequest where encode = genericEncode options

-- | Constructs GetBucketMetricsConfigurationRequest from required parameters
newGetBucketMetricsConfigurationRequest :: BucketName -> MetricsId -> GetBucketMetricsConfigurationRequest
newGetBucketMetricsConfigurationRequest _Bucket _Id = GetBucketMetricsConfigurationRequest { "Bucket": _Bucket, "Id": _Id }

-- | Constructs GetBucketMetricsConfigurationRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newGetBucketMetricsConfigurationRequest' :: BucketName -> MetricsId -> ( { "Bucket" :: (BucketName) , "Id" :: (MetricsId) } -> {"Bucket" :: (BucketName) , "Id" :: (MetricsId) } ) -> GetBucketMetricsConfigurationRequest
newGetBucketMetricsConfigurationRequest' _Bucket _Id customize = (GetBucketMetricsConfigurationRequest <<< customize) { "Bucket": _Bucket, "Id": _Id }



newtype GetBucketNotificationConfigurationRequest = GetBucketNotificationConfigurationRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeGetBucketNotificationConfigurationRequest :: Newtype GetBucketNotificationConfigurationRequest _
derive instance repGenericGetBucketNotificationConfigurationRequest :: Generic GetBucketNotificationConfigurationRequest _
instance showGetBucketNotificationConfigurationRequest :: Show GetBucketNotificationConfigurationRequest where show = genericShow
instance decodeGetBucketNotificationConfigurationRequest :: Decode GetBucketNotificationConfigurationRequest where decode = genericDecode options
instance encodeGetBucketNotificationConfigurationRequest :: Encode GetBucketNotificationConfigurationRequest where encode = genericEncode options

-- | Constructs GetBucketNotificationConfigurationRequest from required parameters
newGetBucketNotificationConfigurationRequest :: BucketName -> GetBucketNotificationConfigurationRequest
newGetBucketNotificationConfigurationRequest _Bucket = GetBucketNotificationConfigurationRequest { "Bucket": _Bucket }

-- | Constructs GetBucketNotificationConfigurationRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newGetBucketNotificationConfigurationRequest' :: BucketName -> ( { "Bucket" :: (BucketName) } -> {"Bucket" :: (BucketName) } ) -> GetBucketNotificationConfigurationRequest
newGetBucketNotificationConfigurationRequest' _Bucket customize = (GetBucketNotificationConfigurationRequest <<< customize) { "Bucket": _Bucket }



newtype GetBucketPolicyOutput = GetBucketPolicyOutput 
  { "Policy" :: NullOrUndefined (Policy)
  }
derive instance newtypeGetBucketPolicyOutput :: Newtype GetBucketPolicyOutput _
derive instance repGenericGetBucketPolicyOutput :: Generic GetBucketPolicyOutput _
instance showGetBucketPolicyOutput :: Show GetBucketPolicyOutput where show = genericShow
instance decodeGetBucketPolicyOutput :: Decode GetBucketPolicyOutput where decode = genericDecode options
instance encodeGetBucketPolicyOutput :: Encode GetBucketPolicyOutput where encode = genericEncode options

-- | Constructs GetBucketPolicyOutput from required parameters
newGetBucketPolicyOutput :: GetBucketPolicyOutput
newGetBucketPolicyOutput  = GetBucketPolicyOutput { "Policy": (NullOrUndefined Nothing) }

-- | Constructs GetBucketPolicyOutput's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newGetBucketPolicyOutput' :: ( { "Policy" :: NullOrUndefined (Policy) } -> {"Policy" :: NullOrUndefined (Policy) } ) -> GetBucketPolicyOutput
newGetBucketPolicyOutput'  customize = (GetBucketPolicyOutput <<< customize) { "Policy": (NullOrUndefined Nothing) }



newtype GetBucketPolicyRequest = GetBucketPolicyRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeGetBucketPolicyRequest :: Newtype GetBucketPolicyRequest _
derive instance repGenericGetBucketPolicyRequest :: Generic GetBucketPolicyRequest _
instance showGetBucketPolicyRequest :: Show GetBucketPolicyRequest where show = genericShow
instance decodeGetBucketPolicyRequest :: Decode GetBucketPolicyRequest where decode = genericDecode options
instance encodeGetBucketPolicyRequest :: Encode GetBucketPolicyRequest where encode = genericEncode options

-- | Constructs GetBucketPolicyRequest from required parameters
newGetBucketPolicyRequest :: BucketName -> GetBucketPolicyRequest
newGetBucketPolicyRequest _Bucket = GetBucketPolicyRequest { "Bucket": _Bucket }

-- | Constructs GetBucketPolicyRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newGetBucketPolicyRequest' :: BucketName -> ( { "Bucket" :: (BucketName) } -> {"Bucket" :: (BucketName) } ) -> GetBucketPolicyRequest
newGetBucketPolicyRequest' _Bucket customize = (GetBucketPolicyRequest <<< customize) { "Bucket": _Bucket }



newtype GetBucketReplicationOutput = GetBucketReplicationOutput 
  { "ReplicationConfiguration" :: NullOrUndefined (ReplicationConfiguration)
  }
derive instance newtypeGetBucketReplicationOutput :: Newtype GetBucketReplicationOutput _
derive instance repGenericGetBucketReplicationOutput :: Generic GetBucketReplicationOutput _
instance showGetBucketReplicationOutput :: Show GetBucketReplicationOutput where show = genericShow
instance decodeGetBucketReplicationOutput :: Decode GetBucketReplicationOutput where decode = genericDecode options
instance encodeGetBucketReplicationOutput :: Encode GetBucketReplicationOutput where encode = genericEncode options

-- | Constructs GetBucketReplicationOutput from required parameters
newGetBucketReplicationOutput :: GetBucketReplicationOutput
newGetBucketReplicationOutput  = GetBucketReplicationOutput { "ReplicationConfiguration": (NullOrUndefined Nothing) }

-- | Constructs GetBucketReplicationOutput's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newGetBucketReplicationOutput' :: ( { "ReplicationConfiguration" :: NullOrUndefined (ReplicationConfiguration) } -> {"ReplicationConfiguration" :: NullOrUndefined (ReplicationConfiguration) } ) -> GetBucketReplicationOutput
newGetBucketReplicationOutput'  customize = (GetBucketReplicationOutput <<< customize) { "ReplicationConfiguration": (NullOrUndefined Nothing) }



newtype GetBucketReplicationRequest = GetBucketReplicationRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeGetBucketReplicationRequest :: Newtype GetBucketReplicationRequest _
derive instance repGenericGetBucketReplicationRequest :: Generic GetBucketReplicationRequest _
instance showGetBucketReplicationRequest :: Show GetBucketReplicationRequest where show = genericShow
instance decodeGetBucketReplicationRequest :: Decode GetBucketReplicationRequest where decode = genericDecode options
instance encodeGetBucketReplicationRequest :: Encode GetBucketReplicationRequest where encode = genericEncode options

-- | Constructs GetBucketReplicationRequest from required parameters
newGetBucketReplicationRequest :: BucketName -> GetBucketReplicationRequest
newGetBucketReplicationRequest _Bucket = GetBucketReplicationRequest { "Bucket": _Bucket }

-- | Constructs GetBucketReplicationRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newGetBucketReplicationRequest' :: BucketName -> ( { "Bucket" :: (BucketName) } -> {"Bucket" :: (BucketName) } ) -> GetBucketReplicationRequest
newGetBucketReplicationRequest' _Bucket customize = (GetBucketReplicationRequest <<< customize) { "Bucket": _Bucket }



newtype GetBucketRequestPaymentOutput = GetBucketRequestPaymentOutput 
  { "Payer" :: NullOrUndefined (Payer)
  }
derive instance newtypeGetBucketRequestPaymentOutput :: Newtype GetBucketRequestPaymentOutput _
derive instance repGenericGetBucketRequestPaymentOutput :: Generic GetBucketRequestPaymentOutput _
instance showGetBucketRequestPaymentOutput :: Show GetBucketRequestPaymentOutput where show = genericShow
instance decodeGetBucketRequestPaymentOutput :: Decode GetBucketRequestPaymentOutput where decode = genericDecode options
instance encodeGetBucketRequestPaymentOutput :: Encode GetBucketRequestPaymentOutput where encode = genericEncode options

-- | Constructs GetBucketRequestPaymentOutput from required parameters
newGetBucketRequestPaymentOutput :: GetBucketRequestPaymentOutput
newGetBucketRequestPaymentOutput  = GetBucketRequestPaymentOutput { "Payer": (NullOrUndefined Nothing) }

-- | Constructs GetBucketRequestPaymentOutput's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newGetBucketRequestPaymentOutput' :: ( { "Payer" :: NullOrUndefined (Payer) } -> {"Payer" :: NullOrUndefined (Payer) } ) -> GetBucketRequestPaymentOutput
newGetBucketRequestPaymentOutput'  customize = (GetBucketRequestPaymentOutput <<< customize) { "Payer": (NullOrUndefined Nothing) }



newtype GetBucketRequestPaymentRequest = GetBucketRequestPaymentRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeGetBucketRequestPaymentRequest :: Newtype GetBucketRequestPaymentRequest _
derive instance repGenericGetBucketRequestPaymentRequest :: Generic GetBucketRequestPaymentRequest _
instance showGetBucketRequestPaymentRequest :: Show GetBucketRequestPaymentRequest where show = genericShow
instance decodeGetBucketRequestPaymentRequest :: Decode GetBucketRequestPaymentRequest where decode = genericDecode options
instance encodeGetBucketRequestPaymentRequest :: Encode GetBucketRequestPaymentRequest where encode = genericEncode options

-- | Constructs GetBucketRequestPaymentRequest from required parameters
newGetBucketRequestPaymentRequest :: BucketName -> GetBucketRequestPaymentRequest
newGetBucketRequestPaymentRequest _Bucket = GetBucketRequestPaymentRequest { "Bucket": _Bucket }

-- | Constructs GetBucketRequestPaymentRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newGetBucketRequestPaymentRequest' :: BucketName -> ( { "Bucket" :: (BucketName) } -> {"Bucket" :: (BucketName) } ) -> GetBucketRequestPaymentRequest
newGetBucketRequestPaymentRequest' _Bucket customize = (GetBucketRequestPaymentRequest <<< customize) { "Bucket": _Bucket }



newtype GetBucketTaggingOutput = GetBucketTaggingOutput 
  { "TagSet" :: (TagSet)
  }
derive instance newtypeGetBucketTaggingOutput :: Newtype GetBucketTaggingOutput _
derive instance repGenericGetBucketTaggingOutput :: Generic GetBucketTaggingOutput _
instance showGetBucketTaggingOutput :: Show GetBucketTaggingOutput where show = genericShow
instance decodeGetBucketTaggingOutput :: Decode GetBucketTaggingOutput where decode = genericDecode options
instance encodeGetBucketTaggingOutput :: Encode GetBucketTaggingOutput where encode = genericEncode options

-- | Constructs GetBucketTaggingOutput from required parameters
newGetBucketTaggingOutput :: TagSet -> GetBucketTaggingOutput
newGetBucketTaggingOutput _TagSet = GetBucketTaggingOutput { "TagSet": _TagSet }

-- | Constructs GetBucketTaggingOutput's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newGetBucketTaggingOutput' :: TagSet -> ( { "TagSet" :: (TagSet) } -> {"TagSet" :: (TagSet) } ) -> GetBucketTaggingOutput
newGetBucketTaggingOutput' _TagSet customize = (GetBucketTaggingOutput <<< customize) { "TagSet": _TagSet }



newtype GetBucketTaggingRequest = GetBucketTaggingRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeGetBucketTaggingRequest :: Newtype GetBucketTaggingRequest _
derive instance repGenericGetBucketTaggingRequest :: Generic GetBucketTaggingRequest _
instance showGetBucketTaggingRequest :: Show GetBucketTaggingRequest where show = genericShow
instance decodeGetBucketTaggingRequest :: Decode GetBucketTaggingRequest where decode = genericDecode options
instance encodeGetBucketTaggingRequest :: Encode GetBucketTaggingRequest where encode = genericEncode options

-- | Constructs GetBucketTaggingRequest from required parameters
newGetBucketTaggingRequest :: BucketName -> GetBucketTaggingRequest
newGetBucketTaggingRequest _Bucket = GetBucketTaggingRequest { "Bucket": _Bucket }

-- | Constructs GetBucketTaggingRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newGetBucketTaggingRequest' :: BucketName -> ( { "Bucket" :: (BucketName) } -> {"Bucket" :: (BucketName) } ) -> GetBucketTaggingRequest
newGetBucketTaggingRequest' _Bucket customize = (GetBucketTaggingRequest <<< customize) { "Bucket": _Bucket }



newtype GetBucketVersioningOutput = GetBucketVersioningOutput 
  { "Status" :: NullOrUndefined (BucketVersioningStatus)
  , "MFADelete" :: NullOrUndefined (MFADeleteStatus)
  }
derive instance newtypeGetBucketVersioningOutput :: Newtype GetBucketVersioningOutput _
derive instance repGenericGetBucketVersioningOutput :: Generic GetBucketVersioningOutput _
instance showGetBucketVersioningOutput :: Show GetBucketVersioningOutput where show = genericShow
instance decodeGetBucketVersioningOutput :: Decode GetBucketVersioningOutput where decode = genericDecode options
instance encodeGetBucketVersioningOutput :: Encode GetBucketVersioningOutput where encode = genericEncode options

-- | Constructs GetBucketVersioningOutput from required parameters
newGetBucketVersioningOutput :: GetBucketVersioningOutput
newGetBucketVersioningOutput  = GetBucketVersioningOutput { "MFADelete": (NullOrUndefined Nothing), "Status": (NullOrUndefined Nothing) }

-- | Constructs GetBucketVersioningOutput's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newGetBucketVersioningOutput' :: ( { "Status" :: NullOrUndefined (BucketVersioningStatus) , "MFADelete" :: NullOrUndefined (MFADeleteStatus) } -> {"Status" :: NullOrUndefined (BucketVersioningStatus) , "MFADelete" :: NullOrUndefined (MFADeleteStatus) } ) -> GetBucketVersioningOutput
newGetBucketVersioningOutput'  customize = (GetBucketVersioningOutput <<< customize) { "MFADelete": (NullOrUndefined Nothing), "Status": (NullOrUndefined Nothing) }



newtype GetBucketVersioningRequest = GetBucketVersioningRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeGetBucketVersioningRequest :: Newtype GetBucketVersioningRequest _
derive instance repGenericGetBucketVersioningRequest :: Generic GetBucketVersioningRequest _
instance showGetBucketVersioningRequest :: Show GetBucketVersioningRequest where show = genericShow
instance decodeGetBucketVersioningRequest :: Decode GetBucketVersioningRequest where decode = genericDecode options
instance encodeGetBucketVersioningRequest :: Encode GetBucketVersioningRequest where encode = genericEncode options

-- | Constructs GetBucketVersioningRequest from required parameters
newGetBucketVersioningRequest :: BucketName -> GetBucketVersioningRequest
newGetBucketVersioningRequest _Bucket = GetBucketVersioningRequest { "Bucket": _Bucket }

-- | Constructs GetBucketVersioningRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newGetBucketVersioningRequest' :: BucketName -> ( { "Bucket" :: (BucketName) } -> {"Bucket" :: (BucketName) } ) -> GetBucketVersioningRequest
newGetBucketVersioningRequest' _Bucket customize = (GetBucketVersioningRequest <<< customize) { "Bucket": _Bucket }



newtype GetBucketWebsiteOutput = GetBucketWebsiteOutput 
  { "RedirectAllRequestsTo" :: NullOrUndefined (RedirectAllRequestsTo)
  , "IndexDocument" :: NullOrUndefined (IndexDocument)
  , "ErrorDocument" :: NullOrUndefined (ErrorDocument)
  , "RoutingRules" :: NullOrUndefined (RoutingRules)
  }
derive instance newtypeGetBucketWebsiteOutput :: Newtype GetBucketWebsiteOutput _
derive instance repGenericGetBucketWebsiteOutput :: Generic GetBucketWebsiteOutput _
instance showGetBucketWebsiteOutput :: Show GetBucketWebsiteOutput where show = genericShow
instance decodeGetBucketWebsiteOutput :: Decode GetBucketWebsiteOutput where decode = genericDecode options
instance encodeGetBucketWebsiteOutput :: Encode GetBucketWebsiteOutput where encode = genericEncode options

-- | Constructs GetBucketWebsiteOutput from required parameters
newGetBucketWebsiteOutput :: GetBucketWebsiteOutput
newGetBucketWebsiteOutput  = GetBucketWebsiteOutput { "ErrorDocument": (NullOrUndefined Nothing), "IndexDocument": (NullOrUndefined Nothing), "RedirectAllRequestsTo": (NullOrUndefined Nothing), "RoutingRules": (NullOrUndefined Nothing) }

-- | Constructs GetBucketWebsiteOutput's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newGetBucketWebsiteOutput' :: ( { "RedirectAllRequestsTo" :: NullOrUndefined (RedirectAllRequestsTo) , "IndexDocument" :: NullOrUndefined (IndexDocument) , "ErrorDocument" :: NullOrUndefined (ErrorDocument) , "RoutingRules" :: NullOrUndefined (RoutingRules) } -> {"RedirectAllRequestsTo" :: NullOrUndefined (RedirectAllRequestsTo) , "IndexDocument" :: NullOrUndefined (IndexDocument) , "ErrorDocument" :: NullOrUndefined (ErrorDocument) , "RoutingRules" :: NullOrUndefined (RoutingRules) } ) -> GetBucketWebsiteOutput
newGetBucketWebsiteOutput'  customize = (GetBucketWebsiteOutput <<< customize) { "ErrorDocument": (NullOrUndefined Nothing), "IndexDocument": (NullOrUndefined Nothing), "RedirectAllRequestsTo": (NullOrUndefined Nothing), "RoutingRules": (NullOrUndefined Nothing) }



newtype GetBucketWebsiteRequest = GetBucketWebsiteRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeGetBucketWebsiteRequest :: Newtype GetBucketWebsiteRequest _
derive instance repGenericGetBucketWebsiteRequest :: Generic GetBucketWebsiteRequest _
instance showGetBucketWebsiteRequest :: Show GetBucketWebsiteRequest where show = genericShow
instance decodeGetBucketWebsiteRequest :: Decode GetBucketWebsiteRequest where decode = genericDecode options
instance encodeGetBucketWebsiteRequest :: Encode GetBucketWebsiteRequest where encode = genericEncode options

-- | Constructs GetBucketWebsiteRequest from required parameters
newGetBucketWebsiteRequest :: BucketName -> GetBucketWebsiteRequest
newGetBucketWebsiteRequest _Bucket = GetBucketWebsiteRequest { "Bucket": _Bucket }

-- | Constructs GetBucketWebsiteRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newGetBucketWebsiteRequest' :: BucketName -> ( { "Bucket" :: (BucketName) } -> {"Bucket" :: (BucketName) } ) -> GetBucketWebsiteRequest
newGetBucketWebsiteRequest' _Bucket customize = (GetBucketWebsiteRequest <<< customize) { "Bucket": _Bucket }



newtype GetObjectAclOutput = GetObjectAclOutput 
  { "Owner" :: NullOrUndefined (Owner)
  , "Grants" :: NullOrUndefined (Grants)
  , "RequestCharged" :: NullOrUndefined (RequestCharged)
  }
derive instance newtypeGetObjectAclOutput :: Newtype GetObjectAclOutput _
derive instance repGenericGetObjectAclOutput :: Generic GetObjectAclOutput _
instance showGetObjectAclOutput :: Show GetObjectAclOutput where show = genericShow
instance decodeGetObjectAclOutput :: Decode GetObjectAclOutput where decode = genericDecode options
instance encodeGetObjectAclOutput :: Encode GetObjectAclOutput where encode = genericEncode options

-- | Constructs GetObjectAclOutput from required parameters
newGetObjectAclOutput :: GetObjectAclOutput
newGetObjectAclOutput  = GetObjectAclOutput { "Grants": (NullOrUndefined Nothing), "Owner": (NullOrUndefined Nothing), "RequestCharged": (NullOrUndefined Nothing) }

-- | Constructs GetObjectAclOutput's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newGetObjectAclOutput' :: ( { "Owner" :: NullOrUndefined (Owner) , "Grants" :: NullOrUndefined (Grants) , "RequestCharged" :: NullOrUndefined (RequestCharged) } -> {"Owner" :: NullOrUndefined (Owner) , "Grants" :: NullOrUndefined (Grants) , "RequestCharged" :: NullOrUndefined (RequestCharged) } ) -> GetObjectAclOutput
newGetObjectAclOutput'  customize = (GetObjectAclOutput <<< customize) { "Grants": (NullOrUndefined Nothing), "Owner": (NullOrUndefined Nothing), "RequestCharged": (NullOrUndefined Nothing) }



newtype GetObjectAclRequest = GetObjectAclRequest 
  { "Bucket" :: (BucketName)
  , "Key" :: (ObjectKey)
  , "VersionId" :: NullOrUndefined (ObjectVersionId)
  , "RequestPayer" :: NullOrUndefined (RequestPayer)
  }
derive instance newtypeGetObjectAclRequest :: Newtype GetObjectAclRequest _
derive instance repGenericGetObjectAclRequest :: Generic GetObjectAclRequest _
instance showGetObjectAclRequest :: Show GetObjectAclRequest where show = genericShow
instance decodeGetObjectAclRequest :: Decode GetObjectAclRequest where decode = genericDecode options
instance encodeGetObjectAclRequest :: Encode GetObjectAclRequest where encode = genericEncode options

-- | Constructs GetObjectAclRequest from required parameters
newGetObjectAclRequest :: BucketName -> ObjectKey -> GetObjectAclRequest
newGetObjectAclRequest _Bucket _Key = GetObjectAclRequest { "Bucket": _Bucket, "Key": _Key, "RequestPayer": (NullOrUndefined Nothing), "VersionId": (NullOrUndefined Nothing) }

-- | Constructs GetObjectAclRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newGetObjectAclRequest' :: BucketName -> ObjectKey -> ( { "Bucket" :: (BucketName) , "Key" :: (ObjectKey) , "VersionId" :: NullOrUndefined (ObjectVersionId) , "RequestPayer" :: NullOrUndefined (RequestPayer) } -> {"Bucket" :: (BucketName) , "Key" :: (ObjectKey) , "VersionId" :: NullOrUndefined (ObjectVersionId) , "RequestPayer" :: NullOrUndefined (RequestPayer) } ) -> GetObjectAclRequest
newGetObjectAclRequest' _Bucket _Key customize = (GetObjectAclRequest <<< customize) { "Bucket": _Bucket, "Key": _Key, "RequestPayer": (NullOrUndefined Nothing), "VersionId": (NullOrUndefined Nothing) }



newtype GetObjectOutput = GetObjectOutput 
  { "Body" :: NullOrUndefined (Body)
  , "DeleteMarker" :: NullOrUndefined (DeleteMarker)
  , "AcceptRanges" :: NullOrUndefined (AcceptRanges)
  , "Expiration" :: NullOrUndefined (Expiration)
  , "Restore" :: NullOrUndefined (Restore)
  , "LastModified" :: NullOrUndefined (LastModified)
  , "ContentLength" :: NullOrUndefined (ContentLength)
  , "ETag" :: NullOrUndefined (ETag)
  , "MissingMeta" :: NullOrUndefined (MissingMeta)
  , "VersionId" :: NullOrUndefined (ObjectVersionId)
  , "CacheControl" :: NullOrUndefined (CacheControl)
  , "ContentDisposition" :: NullOrUndefined (ContentDisposition)
  , "ContentEncoding" :: NullOrUndefined (ContentEncoding)
  , "ContentLanguage" :: NullOrUndefined (ContentLanguage)
  , "ContentRange" :: NullOrUndefined (ContentRange)
  , "ContentType" :: NullOrUndefined (ContentType)
  , "Expires" :: NullOrUndefined (Expires)
  , "WebsiteRedirectLocation" :: NullOrUndefined (WebsiteRedirectLocation)
  , "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption)
  , "Metadata" :: NullOrUndefined (Metadata)
  , "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm)
  , "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5)
  , "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId)
  , "StorageClass" :: NullOrUndefined (StorageClass)
  , "RequestCharged" :: NullOrUndefined (RequestCharged)
  , "ReplicationStatus" :: NullOrUndefined (ReplicationStatus)
  , "PartsCount" :: NullOrUndefined (PartsCount)
  , "TagCount" :: NullOrUndefined (TagCount)
  }
derive instance newtypeGetObjectOutput :: Newtype GetObjectOutput _
derive instance repGenericGetObjectOutput :: Generic GetObjectOutput _
instance showGetObjectOutput :: Show GetObjectOutput where show = genericShow
instance decodeGetObjectOutput :: Decode GetObjectOutput where decode = genericDecode options
instance encodeGetObjectOutput :: Encode GetObjectOutput where encode = genericEncode options

-- | Constructs GetObjectOutput from required parameters
newGetObjectOutput :: GetObjectOutput
newGetObjectOutput  = GetObjectOutput { "AcceptRanges": (NullOrUndefined Nothing), "Body": (NullOrUndefined Nothing), "CacheControl": (NullOrUndefined Nothing), "ContentDisposition": (NullOrUndefined Nothing), "ContentEncoding": (NullOrUndefined Nothing), "ContentLanguage": (NullOrUndefined Nothing), "ContentLength": (NullOrUndefined Nothing), "ContentRange": (NullOrUndefined Nothing), "ContentType": (NullOrUndefined Nothing), "DeleteMarker": (NullOrUndefined Nothing), "ETag": (NullOrUndefined Nothing), "Expiration": (NullOrUndefined Nothing), "Expires": (NullOrUndefined Nothing), "LastModified": (NullOrUndefined Nothing), "Metadata": (NullOrUndefined Nothing), "MissingMeta": (NullOrUndefined Nothing), "PartsCount": (NullOrUndefined Nothing), "ReplicationStatus": (NullOrUndefined Nothing), "RequestCharged": (NullOrUndefined Nothing), "Restore": (NullOrUndefined Nothing), "SSECustomerAlgorithm": (NullOrUndefined Nothing), "SSECustomerKeyMD5": (NullOrUndefined Nothing), "SSEKMSKeyId": (NullOrUndefined Nothing), "ServerSideEncryption": (NullOrUndefined Nothing), "StorageClass": (NullOrUndefined Nothing), "TagCount": (NullOrUndefined Nothing), "VersionId": (NullOrUndefined Nothing), "WebsiteRedirectLocation": (NullOrUndefined Nothing) }

-- | Constructs GetObjectOutput's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newGetObjectOutput' :: ( { "Body" :: NullOrUndefined (Body) , "DeleteMarker" :: NullOrUndefined (DeleteMarker) , "AcceptRanges" :: NullOrUndefined (AcceptRanges) , "Expiration" :: NullOrUndefined (Expiration) , "Restore" :: NullOrUndefined (Restore) , "LastModified" :: NullOrUndefined (LastModified) , "ContentLength" :: NullOrUndefined (ContentLength) , "ETag" :: NullOrUndefined (ETag) , "MissingMeta" :: NullOrUndefined (MissingMeta) , "VersionId" :: NullOrUndefined (ObjectVersionId) , "CacheControl" :: NullOrUndefined (CacheControl) , "ContentDisposition" :: NullOrUndefined (ContentDisposition) , "ContentEncoding" :: NullOrUndefined (ContentEncoding) , "ContentLanguage" :: NullOrUndefined (ContentLanguage) , "ContentRange" :: NullOrUndefined (ContentRange) , "ContentType" :: NullOrUndefined (ContentType) , "Expires" :: NullOrUndefined (Expires) , "WebsiteRedirectLocation" :: NullOrUndefined (WebsiteRedirectLocation) , "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption) , "Metadata" :: NullOrUndefined (Metadata) , "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm) , "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5) , "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId) , "StorageClass" :: NullOrUndefined (StorageClass) , "RequestCharged" :: NullOrUndefined (RequestCharged) , "ReplicationStatus" :: NullOrUndefined (ReplicationStatus) , "PartsCount" :: NullOrUndefined (PartsCount) , "TagCount" :: NullOrUndefined (TagCount) } -> {"Body" :: NullOrUndefined (Body) , "DeleteMarker" :: NullOrUndefined (DeleteMarker) , "AcceptRanges" :: NullOrUndefined (AcceptRanges) , "Expiration" :: NullOrUndefined (Expiration) , "Restore" :: NullOrUndefined (Restore) , "LastModified" :: NullOrUndefined (LastModified) , "ContentLength" :: NullOrUndefined (ContentLength) , "ETag" :: NullOrUndefined (ETag) , "MissingMeta" :: NullOrUndefined (MissingMeta) , "VersionId" :: NullOrUndefined (ObjectVersionId) , "CacheControl" :: NullOrUndefined (CacheControl) , "ContentDisposition" :: NullOrUndefined (ContentDisposition) , "ContentEncoding" :: NullOrUndefined (ContentEncoding) , "ContentLanguage" :: NullOrUndefined (ContentLanguage) , "ContentRange" :: NullOrUndefined (ContentRange) , "ContentType" :: NullOrUndefined (ContentType) , "Expires" :: NullOrUndefined (Expires) , "WebsiteRedirectLocation" :: NullOrUndefined (WebsiteRedirectLocation) , "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption) , "Metadata" :: NullOrUndefined (Metadata) , "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm) , "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5) , "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId) , "StorageClass" :: NullOrUndefined (StorageClass) , "RequestCharged" :: NullOrUndefined (RequestCharged) , "ReplicationStatus" :: NullOrUndefined (ReplicationStatus) , "PartsCount" :: NullOrUndefined (PartsCount) , "TagCount" :: NullOrUndefined (TagCount) } ) -> GetObjectOutput
newGetObjectOutput'  customize = (GetObjectOutput <<< customize) { "AcceptRanges": (NullOrUndefined Nothing), "Body": (NullOrUndefined Nothing), "CacheControl": (NullOrUndefined Nothing), "ContentDisposition": (NullOrUndefined Nothing), "ContentEncoding": (NullOrUndefined Nothing), "ContentLanguage": (NullOrUndefined Nothing), "ContentLength": (NullOrUndefined Nothing), "ContentRange": (NullOrUndefined Nothing), "ContentType": (NullOrUndefined Nothing), "DeleteMarker": (NullOrUndefined Nothing), "ETag": (NullOrUndefined Nothing), "Expiration": (NullOrUndefined Nothing), "Expires": (NullOrUndefined Nothing), "LastModified": (NullOrUndefined Nothing), "Metadata": (NullOrUndefined Nothing), "MissingMeta": (NullOrUndefined Nothing), "PartsCount": (NullOrUndefined Nothing), "ReplicationStatus": (NullOrUndefined Nothing), "RequestCharged": (NullOrUndefined Nothing), "Restore": (NullOrUndefined Nothing), "SSECustomerAlgorithm": (NullOrUndefined Nothing), "SSECustomerKeyMD5": (NullOrUndefined Nothing), "SSEKMSKeyId": (NullOrUndefined Nothing), "ServerSideEncryption": (NullOrUndefined Nothing), "StorageClass": (NullOrUndefined Nothing), "TagCount": (NullOrUndefined Nothing), "VersionId": (NullOrUndefined Nothing), "WebsiteRedirectLocation": (NullOrUndefined Nothing) }



newtype GetObjectRequest = GetObjectRequest 
  { "Bucket" :: (BucketName)
  , "IfMatch" :: NullOrUndefined (IfMatch)
  , "IfModifiedSince" :: NullOrUndefined (IfModifiedSince)
  , "IfNoneMatch" :: NullOrUndefined (IfNoneMatch)
  , "IfUnmodifiedSince" :: NullOrUndefined (IfUnmodifiedSince)
  , "Key" :: (ObjectKey)
  , "Range" :: NullOrUndefined (Range)
  , "ResponseCacheControl" :: NullOrUndefined (ResponseCacheControl)
  , "ResponseContentDisposition" :: NullOrUndefined (ResponseContentDisposition)
  , "ResponseContentEncoding" :: NullOrUndefined (ResponseContentEncoding)
  , "ResponseContentLanguage" :: NullOrUndefined (ResponseContentLanguage)
  , "ResponseContentType" :: NullOrUndefined (ResponseContentType)
  , "ResponseExpires" :: NullOrUndefined (ResponseExpires)
  , "VersionId" :: NullOrUndefined (ObjectVersionId)
  , "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm)
  , "SSECustomerKey" :: NullOrUndefined (SSECustomerKey)
  , "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5)
  , "RequestPayer" :: NullOrUndefined (RequestPayer)
  , "PartNumber" :: NullOrUndefined (PartNumber)
  }
derive instance newtypeGetObjectRequest :: Newtype GetObjectRequest _
derive instance repGenericGetObjectRequest :: Generic GetObjectRequest _
instance showGetObjectRequest :: Show GetObjectRequest where show = genericShow
instance decodeGetObjectRequest :: Decode GetObjectRequest where decode = genericDecode options
instance encodeGetObjectRequest :: Encode GetObjectRequest where encode = genericEncode options

-- | Constructs GetObjectRequest from required parameters
newGetObjectRequest :: BucketName -> ObjectKey -> GetObjectRequest
newGetObjectRequest _Bucket _Key = GetObjectRequest { "Bucket": _Bucket, "Key": _Key, "IfMatch": (NullOrUndefined Nothing), "IfModifiedSince": (NullOrUndefined Nothing), "IfNoneMatch": (NullOrUndefined Nothing), "IfUnmodifiedSince": (NullOrUndefined Nothing), "PartNumber": (NullOrUndefined Nothing), "Range": (NullOrUndefined Nothing), "RequestPayer": (NullOrUndefined Nothing), "ResponseCacheControl": (NullOrUndefined Nothing), "ResponseContentDisposition": (NullOrUndefined Nothing), "ResponseContentEncoding": (NullOrUndefined Nothing), "ResponseContentLanguage": (NullOrUndefined Nothing), "ResponseContentType": (NullOrUndefined Nothing), "ResponseExpires": (NullOrUndefined Nothing), "SSECustomerAlgorithm": (NullOrUndefined Nothing), "SSECustomerKey": (NullOrUndefined Nothing), "SSECustomerKeyMD5": (NullOrUndefined Nothing), "VersionId": (NullOrUndefined Nothing) }

-- | Constructs GetObjectRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newGetObjectRequest' :: BucketName -> ObjectKey -> ( { "Bucket" :: (BucketName) , "IfMatch" :: NullOrUndefined (IfMatch) , "IfModifiedSince" :: NullOrUndefined (IfModifiedSince) , "IfNoneMatch" :: NullOrUndefined (IfNoneMatch) , "IfUnmodifiedSince" :: NullOrUndefined (IfUnmodifiedSince) , "Key" :: (ObjectKey) , "Range" :: NullOrUndefined (Range) , "ResponseCacheControl" :: NullOrUndefined (ResponseCacheControl) , "ResponseContentDisposition" :: NullOrUndefined (ResponseContentDisposition) , "ResponseContentEncoding" :: NullOrUndefined (ResponseContentEncoding) , "ResponseContentLanguage" :: NullOrUndefined (ResponseContentLanguage) , "ResponseContentType" :: NullOrUndefined (ResponseContentType) , "ResponseExpires" :: NullOrUndefined (ResponseExpires) , "VersionId" :: NullOrUndefined (ObjectVersionId) , "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm) , "SSECustomerKey" :: NullOrUndefined (SSECustomerKey) , "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5) , "RequestPayer" :: NullOrUndefined (RequestPayer) , "PartNumber" :: NullOrUndefined (PartNumber) } -> {"Bucket" :: (BucketName) , "IfMatch" :: NullOrUndefined (IfMatch) , "IfModifiedSince" :: NullOrUndefined (IfModifiedSince) , "IfNoneMatch" :: NullOrUndefined (IfNoneMatch) , "IfUnmodifiedSince" :: NullOrUndefined (IfUnmodifiedSince) , "Key" :: (ObjectKey) , "Range" :: NullOrUndefined (Range) , "ResponseCacheControl" :: NullOrUndefined (ResponseCacheControl) , "ResponseContentDisposition" :: NullOrUndefined (ResponseContentDisposition) , "ResponseContentEncoding" :: NullOrUndefined (ResponseContentEncoding) , "ResponseContentLanguage" :: NullOrUndefined (ResponseContentLanguage) , "ResponseContentType" :: NullOrUndefined (ResponseContentType) , "ResponseExpires" :: NullOrUndefined (ResponseExpires) , "VersionId" :: NullOrUndefined (ObjectVersionId) , "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm) , "SSECustomerKey" :: NullOrUndefined (SSECustomerKey) , "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5) , "RequestPayer" :: NullOrUndefined (RequestPayer) , "PartNumber" :: NullOrUndefined (PartNumber) } ) -> GetObjectRequest
newGetObjectRequest' _Bucket _Key customize = (GetObjectRequest <<< customize) { "Bucket": _Bucket, "Key": _Key, "IfMatch": (NullOrUndefined Nothing), "IfModifiedSince": (NullOrUndefined Nothing), "IfNoneMatch": (NullOrUndefined Nothing), "IfUnmodifiedSince": (NullOrUndefined Nothing), "PartNumber": (NullOrUndefined Nothing), "Range": (NullOrUndefined Nothing), "RequestPayer": (NullOrUndefined Nothing), "ResponseCacheControl": (NullOrUndefined Nothing), "ResponseContentDisposition": (NullOrUndefined Nothing), "ResponseContentEncoding": (NullOrUndefined Nothing), "ResponseContentLanguage": (NullOrUndefined Nothing), "ResponseContentType": (NullOrUndefined Nothing), "ResponseExpires": (NullOrUndefined Nothing), "SSECustomerAlgorithm": (NullOrUndefined Nothing), "SSECustomerKey": (NullOrUndefined Nothing), "SSECustomerKeyMD5": (NullOrUndefined Nothing), "VersionId": (NullOrUndefined Nothing) }



newtype GetObjectTaggingOutput = GetObjectTaggingOutput 
  { "VersionId" :: NullOrUndefined (ObjectVersionId)
  , "TagSet" :: (TagSet)
  }
derive instance newtypeGetObjectTaggingOutput :: Newtype GetObjectTaggingOutput _
derive instance repGenericGetObjectTaggingOutput :: Generic GetObjectTaggingOutput _
instance showGetObjectTaggingOutput :: Show GetObjectTaggingOutput where show = genericShow
instance decodeGetObjectTaggingOutput :: Decode GetObjectTaggingOutput where decode = genericDecode options
instance encodeGetObjectTaggingOutput :: Encode GetObjectTaggingOutput where encode = genericEncode options

-- | Constructs GetObjectTaggingOutput from required parameters
newGetObjectTaggingOutput :: TagSet -> GetObjectTaggingOutput
newGetObjectTaggingOutput _TagSet = GetObjectTaggingOutput { "TagSet": _TagSet, "VersionId": (NullOrUndefined Nothing) }

-- | Constructs GetObjectTaggingOutput's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newGetObjectTaggingOutput' :: TagSet -> ( { "VersionId" :: NullOrUndefined (ObjectVersionId) , "TagSet" :: (TagSet) } -> {"VersionId" :: NullOrUndefined (ObjectVersionId) , "TagSet" :: (TagSet) } ) -> GetObjectTaggingOutput
newGetObjectTaggingOutput' _TagSet customize = (GetObjectTaggingOutput <<< customize) { "TagSet": _TagSet, "VersionId": (NullOrUndefined Nothing) }



newtype GetObjectTaggingRequest = GetObjectTaggingRequest 
  { "Bucket" :: (BucketName)
  , "Key" :: (ObjectKey)
  , "VersionId" :: NullOrUndefined (ObjectVersionId)
  }
derive instance newtypeGetObjectTaggingRequest :: Newtype GetObjectTaggingRequest _
derive instance repGenericGetObjectTaggingRequest :: Generic GetObjectTaggingRequest _
instance showGetObjectTaggingRequest :: Show GetObjectTaggingRequest where show = genericShow
instance decodeGetObjectTaggingRequest :: Decode GetObjectTaggingRequest where decode = genericDecode options
instance encodeGetObjectTaggingRequest :: Encode GetObjectTaggingRequest where encode = genericEncode options

-- | Constructs GetObjectTaggingRequest from required parameters
newGetObjectTaggingRequest :: BucketName -> ObjectKey -> GetObjectTaggingRequest
newGetObjectTaggingRequest _Bucket _Key = GetObjectTaggingRequest { "Bucket": _Bucket, "Key": _Key, "VersionId": (NullOrUndefined Nothing) }

-- | Constructs GetObjectTaggingRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newGetObjectTaggingRequest' :: BucketName -> ObjectKey -> ( { "Bucket" :: (BucketName) , "Key" :: (ObjectKey) , "VersionId" :: NullOrUndefined (ObjectVersionId) } -> {"Bucket" :: (BucketName) , "Key" :: (ObjectKey) , "VersionId" :: NullOrUndefined (ObjectVersionId) } ) -> GetObjectTaggingRequest
newGetObjectTaggingRequest' _Bucket _Key customize = (GetObjectTaggingRequest <<< customize) { "Bucket": _Bucket, "Key": _Key, "VersionId": (NullOrUndefined Nothing) }



newtype GetObjectTorrentOutput = GetObjectTorrentOutput 
  { "Body" :: NullOrUndefined (Body)
  , "RequestCharged" :: NullOrUndefined (RequestCharged)
  }
derive instance newtypeGetObjectTorrentOutput :: Newtype GetObjectTorrentOutput _
derive instance repGenericGetObjectTorrentOutput :: Generic GetObjectTorrentOutput _
instance showGetObjectTorrentOutput :: Show GetObjectTorrentOutput where show = genericShow
instance decodeGetObjectTorrentOutput :: Decode GetObjectTorrentOutput where decode = genericDecode options
instance encodeGetObjectTorrentOutput :: Encode GetObjectTorrentOutput where encode = genericEncode options

-- | Constructs GetObjectTorrentOutput from required parameters
newGetObjectTorrentOutput :: GetObjectTorrentOutput
newGetObjectTorrentOutput  = GetObjectTorrentOutput { "Body": (NullOrUndefined Nothing), "RequestCharged": (NullOrUndefined Nothing) }

-- | Constructs GetObjectTorrentOutput's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newGetObjectTorrentOutput' :: ( { "Body" :: NullOrUndefined (Body) , "RequestCharged" :: NullOrUndefined (RequestCharged) } -> {"Body" :: NullOrUndefined (Body) , "RequestCharged" :: NullOrUndefined (RequestCharged) } ) -> GetObjectTorrentOutput
newGetObjectTorrentOutput'  customize = (GetObjectTorrentOutput <<< customize) { "Body": (NullOrUndefined Nothing), "RequestCharged": (NullOrUndefined Nothing) }



newtype GetObjectTorrentRequest = GetObjectTorrentRequest 
  { "Bucket" :: (BucketName)
  , "Key" :: (ObjectKey)
  , "RequestPayer" :: NullOrUndefined (RequestPayer)
  }
derive instance newtypeGetObjectTorrentRequest :: Newtype GetObjectTorrentRequest _
derive instance repGenericGetObjectTorrentRequest :: Generic GetObjectTorrentRequest _
instance showGetObjectTorrentRequest :: Show GetObjectTorrentRequest where show = genericShow
instance decodeGetObjectTorrentRequest :: Decode GetObjectTorrentRequest where decode = genericDecode options
instance encodeGetObjectTorrentRequest :: Encode GetObjectTorrentRequest where encode = genericEncode options

-- | Constructs GetObjectTorrentRequest from required parameters
newGetObjectTorrentRequest :: BucketName -> ObjectKey -> GetObjectTorrentRequest
newGetObjectTorrentRequest _Bucket _Key = GetObjectTorrentRequest { "Bucket": _Bucket, "Key": _Key, "RequestPayer": (NullOrUndefined Nothing) }

-- | Constructs GetObjectTorrentRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newGetObjectTorrentRequest' :: BucketName -> ObjectKey -> ( { "Bucket" :: (BucketName) , "Key" :: (ObjectKey) , "RequestPayer" :: NullOrUndefined (RequestPayer) } -> {"Bucket" :: (BucketName) , "Key" :: (ObjectKey) , "RequestPayer" :: NullOrUndefined (RequestPayer) } ) -> GetObjectTorrentRequest
newGetObjectTorrentRequest' _Bucket _Key customize = (GetObjectTorrentRequest <<< customize) { "Bucket": _Bucket, "Key": _Key, "RequestPayer": (NullOrUndefined Nothing) }



newtype GlacierJobParameters = GlacierJobParameters 
  { "Tier" :: (Tier)
  }
derive instance newtypeGlacierJobParameters :: Newtype GlacierJobParameters _
derive instance repGenericGlacierJobParameters :: Generic GlacierJobParameters _
instance showGlacierJobParameters :: Show GlacierJobParameters where show = genericShow
instance decodeGlacierJobParameters :: Decode GlacierJobParameters where decode = genericDecode options
instance encodeGlacierJobParameters :: Encode GlacierJobParameters where encode = genericEncode options

-- | Constructs GlacierJobParameters from required parameters
newGlacierJobParameters :: Tier -> GlacierJobParameters
newGlacierJobParameters _Tier = GlacierJobParameters { "Tier": _Tier }

-- | Constructs GlacierJobParameters's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newGlacierJobParameters' :: Tier -> ( { "Tier" :: (Tier) } -> {"Tier" :: (Tier) } ) -> GlacierJobParameters
newGlacierJobParameters' _Tier customize = (GlacierJobParameters <<< customize) { "Tier": _Tier }



newtype Grant = Grant 
  { "Grantee" :: NullOrUndefined (Grantee)
  , "Permission" :: NullOrUndefined (Permission)
  }
derive instance newtypeGrant :: Newtype Grant _
derive instance repGenericGrant :: Generic Grant _
instance showGrant :: Show Grant where show = genericShow
instance decodeGrant :: Decode Grant where decode = genericDecode options
instance encodeGrant :: Encode Grant where encode = genericEncode options

-- | Constructs Grant from required parameters
newGrant :: Grant
newGrant  = Grant { "Grantee": (NullOrUndefined Nothing), "Permission": (NullOrUndefined Nothing) }

-- | Constructs Grant's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newGrant' :: ( { "Grantee" :: NullOrUndefined (Grantee) , "Permission" :: NullOrUndefined (Permission) } -> {"Grantee" :: NullOrUndefined (Grantee) , "Permission" :: NullOrUndefined (Permission) } ) -> Grant
newGrant'  customize = (Grant <<< customize) { "Grantee": (NullOrUndefined Nothing), "Permission": (NullOrUndefined Nothing) }



newtype GrantFullControl = GrantFullControl String
derive instance newtypeGrantFullControl :: Newtype GrantFullControl _
derive instance repGenericGrantFullControl :: Generic GrantFullControl _
instance showGrantFullControl :: Show GrantFullControl where show = genericShow
instance decodeGrantFullControl :: Decode GrantFullControl where decode = genericDecode options
instance encodeGrantFullControl :: Encode GrantFullControl where encode = genericEncode options



newtype GrantRead = GrantRead String
derive instance newtypeGrantRead :: Newtype GrantRead _
derive instance repGenericGrantRead :: Generic GrantRead _
instance showGrantRead :: Show GrantRead where show = genericShow
instance decodeGrantRead :: Decode GrantRead where decode = genericDecode options
instance encodeGrantRead :: Encode GrantRead where encode = genericEncode options



newtype GrantReadACP = GrantReadACP String
derive instance newtypeGrantReadACP :: Newtype GrantReadACP _
derive instance repGenericGrantReadACP :: Generic GrantReadACP _
instance showGrantReadACP :: Show GrantReadACP where show = genericShow
instance decodeGrantReadACP :: Decode GrantReadACP where decode = genericDecode options
instance encodeGrantReadACP :: Encode GrantReadACP where encode = genericEncode options



newtype GrantWrite = GrantWrite String
derive instance newtypeGrantWrite :: Newtype GrantWrite _
derive instance repGenericGrantWrite :: Generic GrantWrite _
instance showGrantWrite :: Show GrantWrite where show = genericShow
instance decodeGrantWrite :: Decode GrantWrite where decode = genericDecode options
instance encodeGrantWrite :: Encode GrantWrite where encode = genericEncode options



newtype GrantWriteACP = GrantWriteACP String
derive instance newtypeGrantWriteACP :: Newtype GrantWriteACP _
derive instance repGenericGrantWriteACP :: Generic GrantWriteACP _
instance showGrantWriteACP :: Show GrantWriteACP where show = genericShow
instance decodeGrantWriteACP :: Decode GrantWriteACP where decode = genericDecode options
instance encodeGrantWriteACP :: Encode GrantWriteACP where encode = genericEncode options



newtype Grantee = Grantee 
  { "DisplayName" :: NullOrUndefined (DisplayName)
  , "EmailAddress" :: NullOrUndefined (EmailAddress)
  , "ID" :: NullOrUndefined (ID)
  , "Type" :: (Type)
  , "URI" :: NullOrUndefined (URI)
  }
derive instance newtypeGrantee :: Newtype Grantee _
derive instance repGenericGrantee :: Generic Grantee _
instance showGrantee :: Show Grantee where show = genericShow
instance decodeGrantee :: Decode Grantee where decode = genericDecode options
instance encodeGrantee :: Encode Grantee where encode = genericEncode options

-- | Constructs Grantee from required parameters
newGrantee :: Type -> Grantee
newGrantee _Type = Grantee { "Type": _Type, "DisplayName": (NullOrUndefined Nothing), "EmailAddress": (NullOrUndefined Nothing), "ID": (NullOrUndefined Nothing), "URI": (NullOrUndefined Nothing) }

-- | Constructs Grantee's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newGrantee' :: Type -> ( { "DisplayName" :: NullOrUndefined (DisplayName) , "EmailAddress" :: NullOrUndefined (EmailAddress) , "ID" :: NullOrUndefined (ID) , "Type" :: (Type) , "URI" :: NullOrUndefined (URI) } -> {"DisplayName" :: NullOrUndefined (DisplayName) , "EmailAddress" :: NullOrUndefined (EmailAddress) , "ID" :: NullOrUndefined (ID) , "Type" :: (Type) , "URI" :: NullOrUndefined (URI) } ) -> Grantee
newGrantee' _Type customize = (Grantee <<< customize) { "Type": _Type, "DisplayName": (NullOrUndefined Nothing), "EmailAddress": (NullOrUndefined Nothing), "ID": (NullOrUndefined Nothing), "URI": (NullOrUndefined Nothing) }



newtype Grants = Grants (Array Grant)
derive instance newtypeGrants :: Newtype Grants _
derive instance repGenericGrants :: Generic Grants _
instance showGrants :: Show Grants where show = genericShow
instance decodeGrants :: Decode Grants where decode = genericDecode options
instance encodeGrants :: Encode Grants where encode = genericEncode options



newtype HeadBucketRequest = HeadBucketRequest 
  { "Bucket" :: (BucketName)
  }
derive instance newtypeHeadBucketRequest :: Newtype HeadBucketRequest _
derive instance repGenericHeadBucketRequest :: Generic HeadBucketRequest _
instance showHeadBucketRequest :: Show HeadBucketRequest where show = genericShow
instance decodeHeadBucketRequest :: Decode HeadBucketRequest where decode = genericDecode options
instance encodeHeadBucketRequest :: Encode HeadBucketRequest where encode = genericEncode options

-- | Constructs HeadBucketRequest from required parameters
newHeadBucketRequest :: BucketName -> HeadBucketRequest
newHeadBucketRequest _Bucket = HeadBucketRequest { "Bucket": _Bucket }

-- | Constructs HeadBucketRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newHeadBucketRequest' :: BucketName -> ( { "Bucket" :: (BucketName) } -> {"Bucket" :: (BucketName) } ) -> HeadBucketRequest
newHeadBucketRequest' _Bucket customize = (HeadBucketRequest <<< customize) { "Bucket": _Bucket }



newtype HeadObjectOutput = HeadObjectOutput 
  { "DeleteMarker" :: NullOrUndefined (DeleteMarker)
  , "AcceptRanges" :: NullOrUndefined (AcceptRanges)
  , "Expiration" :: NullOrUndefined (Expiration)
  , "Restore" :: NullOrUndefined (Restore)
  , "LastModified" :: NullOrUndefined (LastModified)
  , "ContentLength" :: NullOrUndefined (ContentLength)
  , "ETag" :: NullOrUndefined (ETag)
  , "MissingMeta" :: NullOrUndefined (MissingMeta)
  , "VersionId" :: NullOrUndefined (ObjectVersionId)
  , "CacheControl" :: NullOrUndefined (CacheControl)
  , "ContentDisposition" :: NullOrUndefined (ContentDisposition)
  , "ContentEncoding" :: NullOrUndefined (ContentEncoding)
  , "ContentLanguage" :: NullOrUndefined (ContentLanguage)
  , "ContentType" :: NullOrUndefined (ContentType)
  , "Expires" :: NullOrUndefined (Expires)
  , "WebsiteRedirectLocation" :: NullOrUndefined (WebsiteRedirectLocation)
  , "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption)
  , "Metadata" :: NullOrUndefined (Metadata)
  , "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm)
  , "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5)
  , "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId)
  , "StorageClass" :: NullOrUndefined (StorageClass)
  , "RequestCharged" :: NullOrUndefined (RequestCharged)
  , "ReplicationStatus" :: NullOrUndefined (ReplicationStatus)
  , "PartsCount" :: NullOrUndefined (PartsCount)
  }
derive instance newtypeHeadObjectOutput :: Newtype HeadObjectOutput _
derive instance repGenericHeadObjectOutput :: Generic HeadObjectOutput _
instance showHeadObjectOutput :: Show HeadObjectOutput where show = genericShow
instance decodeHeadObjectOutput :: Decode HeadObjectOutput where decode = genericDecode options
instance encodeHeadObjectOutput :: Encode HeadObjectOutput where encode = genericEncode options

-- | Constructs HeadObjectOutput from required parameters
newHeadObjectOutput :: HeadObjectOutput
newHeadObjectOutput  = HeadObjectOutput { "AcceptRanges": (NullOrUndefined Nothing), "CacheControl": (NullOrUndefined Nothing), "ContentDisposition": (NullOrUndefined Nothing), "ContentEncoding": (NullOrUndefined Nothing), "ContentLanguage": (NullOrUndefined Nothing), "ContentLength": (NullOrUndefined Nothing), "ContentType": (NullOrUndefined Nothing), "DeleteMarker": (NullOrUndefined Nothing), "ETag": (NullOrUndefined Nothing), "Expiration": (NullOrUndefined Nothing), "Expires": (NullOrUndefined Nothing), "LastModified": (NullOrUndefined Nothing), "Metadata": (NullOrUndefined Nothing), "MissingMeta": (NullOrUndefined Nothing), "PartsCount": (NullOrUndefined Nothing), "ReplicationStatus": (NullOrUndefined Nothing), "RequestCharged": (NullOrUndefined Nothing), "Restore": (NullOrUndefined Nothing), "SSECustomerAlgorithm": (NullOrUndefined Nothing), "SSECustomerKeyMD5": (NullOrUndefined Nothing), "SSEKMSKeyId": (NullOrUndefined Nothing), "ServerSideEncryption": (NullOrUndefined Nothing), "StorageClass": (NullOrUndefined Nothing), "VersionId": (NullOrUndefined Nothing), "WebsiteRedirectLocation": (NullOrUndefined Nothing) }

-- | Constructs HeadObjectOutput's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newHeadObjectOutput' :: ( { "DeleteMarker" :: NullOrUndefined (DeleteMarker) , "AcceptRanges" :: NullOrUndefined (AcceptRanges) , "Expiration" :: NullOrUndefined (Expiration) , "Restore" :: NullOrUndefined (Restore) , "LastModified" :: NullOrUndefined (LastModified) , "ContentLength" :: NullOrUndefined (ContentLength) , "ETag" :: NullOrUndefined (ETag) , "MissingMeta" :: NullOrUndefined (MissingMeta) , "VersionId" :: NullOrUndefined (ObjectVersionId) , "CacheControl" :: NullOrUndefined (CacheControl) , "ContentDisposition" :: NullOrUndefined (ContentDisposition) , "ContentEncoding" :: NullOrUndefined (ContentEncoding) , "ContentLanguage" :: NullOrUndefined (ContentLanguage) , "ContentType" :: NullOrUndefined (ContentType) , "Expires" :: NullOrUndefined (Expires) , "WebsiteRedirectLocation" :: NullOrUndefined (WebsiteRedirectLocation) , "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption) , "Metadata" :: NullOrUndefined (Metadata) , "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm) , "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5) , "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId) , "StorageClass" :: NullOrUndefined (StorageClass) , "RequestCharged" :: NullOrUndefined (RequestCharged) , "ReplicationStatus" :: NullOrUndefined (ReplicationStatus) , "PartsCount" :: NullOrUndefined (PartsCount) } -> {"DeleteMarker" :: NullOrUndefined (DeleteMarker) , "AcceptRanges" :: NullOrUndefined (AcceptRanges) , "Expiration" :: NullOrUndefined (Expiration) , "Restore" :: NullOrUndefined (Restore) , "LastModified" :: NullOrUndefined (LastModified) , "ContentLength" :: NullOrUndefined (ContentLength) , "ETag" :: NullOrUndefined (ETag) , "MissingMeta" :: NullOrUndefined (MissingMeta) , "VersionId" :: NullOrUndefined (ObjectVersionId) , "CacheControl" :: NullOrUndefined (CacheControl) , "ContentDisposition" :: NullOrUndefined (ContentDisposition) , "ContentEncoding" :: NullOrUndefined (ContentEncoding) , "ContentLanguage" :: NullOrUndefined (ContentLanguage) , "ContentType" :: NullOrUndefined (ContentType) , "Expires" :: NullOrUndefined (Expires) , "WebsiteRedirectLocation" :: NullOrUndefined (WebsiteRedirectLocation) , "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption) , "Metadata" :: NullOrUndefined (Metadata) , "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm) , "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5) , "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId) , "StorageClass" :: NullOrUndefined (StorageClass) , "RequestCharged" :: NullOrUndefined (RequestCharged) , "ReplicationStatus" :: NullOrUndefined (ReplicationStatus) , "PartsCount" :: NullOrUndefined (PartsCount) } ) -> HeadObjectOutput
newHeadObjectOutput'  customize = (HeadObjectOutput <<< customize) { "AcceptRanges": (NullOrUndefined Nothing), "CacheControl": (NullOrUndefined Nothing), "ContentDisposition": (NullOrUndefined Nothing), "ContentEncoding": (NullOrUndefined Nothing), "ContentLanguage": (NullOrUndefined Nothing), "ContentLength": (NullOrUndefined Nothing), "ContentType": (NullOrUndefined Nothing), "DeleteMarker": (NullOrUndefined Nothing), "ETag": (NullOrUndefined Nothing), "Expiration": (NullOrUndefined Nothing), "Expires": (NullOrUndefined Nothing), "LastModified": (NullOrUndefined Nothing), "Metadata": (NullOrUndefined Nothing), "MissingMeta": (NullOrUndefined Nothing), "PartsCount": (NullOrUndefined Nothing), "ReplicationStatus": (NullOrUndefined Nothing), "RequestCharged": (NullOrUndefined Nothing), "Restore": (NullOrUndefined Nothing), "SSECustomerAlgorithm": (NullOrUndefined Nothing), "SSECustomerKeyMD5": (NullOrUndefined Nothing), "SSEKMSKeyId": (NullOrUndefined Nothing), "ServerSideEncryption": (NullOrUndefined Nothing), "StorageClass": (NullOrUndefined Nothing), "VersionId": (NullOrUndefined Nothing), "WebsiteRedirectLocation": (NullOrUndefined Nothing) }



newtype HeadObjectRequest = HeadObjectRequest 
  { "Bucket" :: (BucketName)
  , "IfMatch" :: NullOrUndefined (IfMatch)
  , "IfModifiedSince" :: NullOrUndefined (IfModifiedSince)
  , "IfNoneMatch" :: NullOrUndefined (IfNoneMatch)
  , "IfUnmodifiedSince" :: NullOrUndefined (IfUnmodifiedSince)
  , "Key" :: (ObjectKey)
  , "Range" :: NullOrUndefined (Range)
  , "VersionId" :: NullOrUndefined (ObjectVersionId)
  , "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm)
  , "SSECustomerKey" :: NullOrUndefined (SSECustomerKey)
  , "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5)
  , "RequestPayer" :: NullOrUndefined (RequestPayer)
  , "PartNumber" :: NullOrUndefined (PartNumber)
  }
derive instance newtypeHeadObjectRequest :: Newtype HeadObjectRequest _
derive instance repGenericHeadObjectRequest :: Generic HeadObjectRequest _
instance showHeadObjectRequest :: Show HeadObjectRequest where show = genericShow
instance decodeHeadObjectRequest :: Decode HeadObjectRequest where decode = genericDecode options
instance encodeHeadObjectRequest :: Encode HeadObjectRequest where encode = genericEncode options

-- | Constructs HeadObjectRequest from required parameters
newHeadObjectRequest :: BucketName -> ObjectKey -> HeadObjectRequest
newHeadObjectRequest _Bucket _Key = HeadObjectRequest { "Bucket": _Bucket, "Key": _Key, "IfMatch": (NullOrUndefined Nothing), "IfModifiedSince": (NullOrUndefined Nothing), "IfNoneMatch": (NullOrUndefined Nothing), "IfUnmodifiedSince": (NullOrUndefined Nothing), "PartNumber": (NullOrUndefined Nothing), "Range": (NullOrUndefined Nothing), "RequestPayer": (NullOrUndefined Nothing), "SSECustomerAlgorithm": (NullOrUndefined Nothing), "SSECustomerKey": (NullOrUndefined Nothing), "SSECustomerKeyMD5": (NullOrUndefined Nothing), "VersionId": (NullOrUndefined Nothing) }

-- | Constructs HeadObjectRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newHeadObjectRequest' :: BucketName -> ObjectKey -> ( { "Bucket" :: (BucketName) , "IfMatch" :: NullOrUndefined (IfMatch) , "IfModifiedSince" :: NullOrUndefined (IfModifiedSince) , "IfNoneMatch" :: NullOrUndefined (IfNoneMatch) , "IfUnmodifiedSince" :: NullOrUndefined (IfUnmodifiedSince) , "Key" :: (ObjectKey) , "Range" :: NullOrUndefined (Range) , "VersionId" :: NullOrUndefined (ObjectVersionId) , "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm) , "SSECustomerKey" :: NullOrUndefined (SSECustomerKey) , "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5) , "RequestPayer" :: NullOrUndefined (RequestPayer) , "PartNumber" :: NullOrUndefined (PartNumber) } -> {"Bucket" :: (BucketName) , "IfMatch" :: NullOrUndefined (IfMatch) , "IfModifiedSince" :: NullOrUndefined (IfModifiedSince) , "IfNoneMatch" :: NullOrUndefined (IfNoneMatch) , "IfUnmodifiedSince" :: NullOrUndefined (IfUnmodifiedSince) , "Key" :: (ObjectKey) , "Range" :: NullOrUndefined (Range) , "VersionId" :: NullOrUndefined (ObjectVersionId) , "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm) , "SSECustomerKey" :: NullOrUndefined (SSECustomerKey) , "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5) , "RequestPayer" :: NullOrUndefined (RequestPayer) , "PartNumber" :: NullOrUndefined (PartNumber) } ) -> HeadObjectRequest
newHeadObjectRequest' _Bucket _Key customize = (HeadObjectRequest <<< customize) { "Bucket": _Bucket, "Key": _Key, "IfMatch": (NullOrUndefined Nothing), "IfModifiedSince": (NullOrUndefined Nothing), "IfNoneMatch": (NullOrUndefined Nothing), "IfUnmodifiedSince": (NullOrUndefined Nothing), "PartNumber": (NullOrUndefined Nothing), "Range": (NullOrUndefined Nothing), "RequestPayer": (NullOrUndefined Nothing), "SSECustomerAlgorithm": (NullOrUndefined Nothing), "SSECustomerKey": (NullOrUndefined Nothing), "SSECustomerKeyMD5": (NullOrUndefined Nothing), "VersionId": (NullOrUndefined Nothing) }



newtype HostName = HostName String
derive instance newtypeHostName :: Newtype HostName _
derive instance repGenericHostName :: Generic HostName _
instance showHostName :: Show HostName where show = genericShow
instance decodeHostName :: Decode HostName where decode = genericDecode options
instance encodeHostName :: Encode HostName where encode = genericEncode options



newtype HttpErrorCodeReturnedEquals = HttpErrorCodeReturnedEquals String
derive instance newtypeHttpErrorCodeReturnedEquals :: Newtype HttpErrorCodeReturnedEquals _
derive instance repGenericHttpErrorCodeReturnedEquals :: Generic HttpErrorCodeReturnedEquals _
instance showHttpErrorCodeReturnedEquals :: Show HttpErrorCodeReturnedEquals where show = genericShow
instance decodeHttpErrorCodeReturnedEquals :: Decode HttpErrorCodeReturnedEquals where decode = genericDecode options
instance encodeHttpErrorCodeReturnedEquals :: Encode HttpErrorCodeReturnedEquals where encode = genericEncode options



newtype HttpRedirectCode = HttpRedirectCode String
derive instance newtypeHttpRedirectCode :: Newtype HttpRedirectCode _
derive instance repGenericHttpRedirectCode :: Generic HttpRedirectCode _
instance showHttpRedirectCode :: Show HttpRedirectCode where show = genericShow
instance decodeHttpRedirectCode :: Decode HttpRedirectCode where decode = genericDecode options
instance encodeHttpRedirectCode :: Encode HttpRedirectCode where encode = genericEncode options



newtype ID = ID String
derive instance newtypeID :: Newtype ID _
derive instance repGenericID :: Generic ID _
instance showID :: Show ID where show = genericShow
instance decodeID :: Decode ID where decode = genericDecode options
instance encodeID :: Encode ID where encode = genericEncode options



newtype IfMatch = IfMatch String
derive instance newtypeIfMatch :: Newtype IfMatch _
derive instance repGenericIfMatch :: Generic IfMatch _
instance showIfMatch :: Show IfMatch where show = genericShow
instance decodeIfMatch :: Decode IfMatch where decode = genericDecode options
instance encodeIfMatch :: Encode IfMatch where encode = genericEncode options



newtype IfModifiedSince = IfModifiedSince Types.Timestamp
derive instance newtypeIfModifiedSince :: Newtype IfModifiedSince _
derive instance repGenericIfModifiedSince :: Generic IfModifiedSince _
instance showIfModifiedSince :: Show IfModifiedSince where show = genericShow
instance decodeIfModifiedSince :: Decode IfModifiedSince where decode = genericDecode options
instance encodeIfModifiedSince :: Encode IfModifiedSince where encode = genericEncode options



newtype IfNoneMatch = IfNoneMatch String
derive instance newtypeIfNoneMatch :: Newtype IfNoneMatch _
derive instance repGenericIfNoneMatch :: Generic IfNoneMatch _
instance showIfNoneMatch :: Show IfNoneMatch where show = genericShow
instance decodeIfNoneMatch :: Decode IfNoneMatch where decode = genericDecode options
instance encodeIfNoneMatch :: Encode IfNoneMatch where encode = genericEncode options



newtype IfUnmodifiedSince = IfUnmodifiedSince Types.Timestamp
derive instance newtypeIfUnmodifiedSince :: Newtype IfUnmodifiedSince _
derive instance repGenericIfUnmodifiedSince :: Generic IfUnmodifiedSince _
instance showIfUnmodifiedSince :: Show IfUnmodifiedSince where show = genericShow
instance decodeIfUnmodifiedSince :: Decode IfUnmodifiedSince where decode = genericDecode options
instance encodeIfUnmodifiedSince :: Encode IfUnmodifiedSince where encode = genericEncode options



newtype IndexDocument = IndexDocument 
  { "Suffix" :: (Suffix)
  }
derive instance newtypeIndexDocument :: Newtype IndexDocument _
derive instance repGenericIndexDocument :: Generic IndexDocument _
instance showIndexDocument :: Show IndexDocument where show = genericShow
instance decodeIndexDocument :: Decode IndexDocument where decode = genericDecode options
instance encodeIndexDocument :: Encode IndexDocument where encode = genericEncode options

-- | Constructs IndexDocument from required parameters
newIndexDocument :: Suffix -> IndexDocument
newIndexDocument _Suffix = IndexDocument { "Suffix": _Suffix }

-- | Constructs IndexDocument's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newIndexDocument' :: Suffix -> ( { "Suffix" :: (Suffix) } -> {"Suffix" :: (Suffix) } ) -> IndexDocument
newIndexDocument' _Suffix customize = (IndexDocument <<< customize) { "Suffix": _Suffix }



newtype Initiated = Initiated Types.Timestamp
derive instance newtypeInitiated :: Newtype Initiated _
derive instance repGenericInitiated :: Generic Initiated _
instance showInitiated :: Show Initiated where show = genericShow
instance decodeInitiated :: Decode Initiated where decode = genericDecode options
instance encodeInitiated :: Encode Initiated where encode = genericEncode options



newtype Initiator = Initiator 
  { "ID" :: NullOrUndefined (ID)
  , "DisplayName" :: NullOrUndefined (DisplayName)
  }
derive instance newtypeInitiator :: Newtype Initiator _
derive instance repGenericInitiator :: Generic Initiator _
instance showInitiator :: Show Initiator where show = genericShow
instance decodeInitiator :: Decode Initiator where decode = genericDecode options
instance encodeInitiator :: Encode Initiator where encode = genericEncode options

-- | Constructs Initiator from required parameters
newInitiator :: Initiator
newInitiator  = Initiator { "DisplayName": (NullOrUndefined Nothing), "ID": (NullOrUndefined Nothing) }

-- | Constructs Initiator's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newInitiator' :: ( { "ID" :: NullOrUndefined (ID) , "DisplayName" :: NullOrUndefined (DisplayName) } -> {"ID" :: NullOrUndefined (ID) , "DisplayName" :: NullOrUndefined (DisplayName) } ) -> Initiator
newInitiator'  customize = (Initiator <<< customize) { "DisplayName": (NullOrUndefined Nothing), "ID": (NullOrUndefined Nothing) }



-- | Describes the serialization format of the object.
newtype InputSerialization = InputSerialization 
  { "CSV" :: NullOrUndefined (CSVInput)
  }
derive instance newtypeInputSerialization :: Newtype InputSerialization _
derive instance repGenericInputSerialization :: Generic InputSerialization _
instance showInputSerialization :: Show InputSerialization where show = genericShow
instance decodeInputSerialization :: Decode InputSerialization where decode = genericDecode options
instance encodeInputSerialization :: Encode InputSerialization where encode = genericEncode options

-- | Constructs InputSerialization from required parameters
newInputSerialization :: InputSerialization
newInputSerialization  = InputSerialization { "CSV": (NullOrUndefined Nothing) }

-- | Constructs InputSerialization's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newInputSerialization' :: ( { "CSV" :: NullOrUndefined (CSVInput) } -> {"CSV" :: NullOrUndefined (CSVInput) } ) -> InputSerialization
newInputSerialization'  customize = (InputSerialization <<< customize) { "CSV": (NullOrUndefined Nothing) }



newtype InventoryConfiguration = InventoryConfiguration 
  { "Destination" :: (InventoryDestination)
  , "IsEnabled" :: (IsEnabled)
  , "Filter" :: NullOrUndefined (InventoryFilter)
  , "Id" :: (InventoryId)
  , "IncludedObjectVersions" :: (InventoryIncludedObjectVersions)
  , "OptionalFields" :: NullOrUndefined (InventoryOptionalFields)
  , "Schedule" :: (InventorySchedule)
  }
derive instance newtypeInventoryConfiguration :: Newtype InventoryConfiguration _
derive instance repGenericInventoryConfiguration :: Generic InventoryConfiguration _
instance showInventoryConfiguration :: Show InventoryConfiguration where show = genericShow
instance decodeInventoryConfiguration :: Decode InventoryConfiguration where decode = genericDecode options
instance encodeInventoryConfiguration :: Encode InventoryConfiguration where encode = genericEncode options

-- | Constructs InventoryConfiguration from required parameters
newInventoryConfiguration :: InventoryDestination -> InventoryId -> InventoryIncludedObjectVersions -> IsEnabled -> InventorySchedule -> InventoryConfiguration
newInventoryConfiguration _Destination _Id _IncludedObjectVersions _IsEnabled _Schedule = InventoryConfiguration { "Destination": _Destination, "Id": _Id, "IncludedObjectVersions": _IncludedObjectVersions, "IsEnabled": _IsEnabled, "Schedule": _Schedule, "Filter": (NullOrUndefined Nothing), "OptionalFields": (NullOrUndefined Nothing) }

-- | Constructs InventoryConfiguration's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newInventoryConfiguration' :: InventoryDestination -> InventoryId -> InventoryIncludedObjectVersions -> IsEnabled -> InventorySchedule -> ( { "Destination" :: (InventoryDestination) , "IsEnabled" :: (IsEnabled) , "Filter" :: NullOrUndefined (InventoryFilter) , "Id" :: (InventoryId) , "IncludedObjectVersions" :: (InventoryIncludedObjectVersions) , "OptionalFields" :: NullOrUndefined (InventoryOptionalFields) , "Schedule" :: (InventorySchedule) } -> {"Destination" :: (InventoryDestination) , "IsEnabled" :: (IsEnabled) , "Filter" :: NullOrUndefined (InventoryFilter) , "Id" :: (InventoryId) , "IncludedObjectVersions" :: (InventoryIncludedObjectVersions) , "OptionalFields" :: NullOrUndefined (InventoryOptionalFields) , "Schedule" :: (InventorySchedule) } ) -> InventoryConfiguration
newInventoryConfiguration' _Destination _Id _IncludedObjectVersions _IsEnabled _Schedule customize = (InventoryConfiguration <<< customize) { "Destination": _Destination, "Id": _Id, "IncludedObjectVersions": _IncludedObjectVersions, "IsEnabled": _IsEnabled, "Schedule": _Schedule, "Filter": (NullOrUndefined Nothing), "OptionalFields": (NullOrUndefined Nothing) }



newtype InventoryConfigurationList = InventoryConfigurationList (Array InventoryConfiguration)
derive instance newtypeInventoryConfigurationList :: Newtype InventoryConfigurationList _
derive instance repGenericInventoryConfigurationList :: Generic InventoryConfigurationList _
instance showInventoryConfigurationList :: Show InventoryConfigurationList where show = genericShow
instance decodeInventoryConfigurationList :: Decode InventoryConfigurationList where decode = genericDecode options
instance encodeInventoryConfigurationList :: Encode InventoryConfigurationList where encode = genericEncode options



newtype InventoryDestination = InventoryDestination 
  { "S3BucketDestination" :: (InventoryS3BucketDestination)
  }
derive instance newtypeInventoryDestination :: Newtype InventoryDestination _
derive instance repGenericInventoryDestination :: Generic InventoryDestination _
instance showInventoryDestination :: Show InventoryDestination where show = genericShow
instance decodeInventoryDestination :: Decode InventoryDestination where decode = genericDecode options
instance encodeInventoryDestination :: Encode InventoryDestination where encode = genericEncode options

-- | Constructs InventoryDestination from required parameters
newInventoryDestination :: InventoryS3BucketDestination -> InventoryDestination
newInventoryDestination _S3BucketDestination = InventoryDestination { "S3BucketDestination": _S3BucketDestination }

-- | Constructs InventoryDestination's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newInventoryDestination' :: InventoryS3BucketDestination -> ( { "S3BucketDestination" :: (InventoryS3BucketDestination) } -> {"S3BucketDestination" :: (InventoryS3BucketDestination) } ) -> InventoryDestination
newInventoryDestination' _S3BucketDestination customize = (InventoryDestination <<< customize) { "S3BucketDestination": _S3BucketDestination }



-- | Contains the type of server-side encryption used to encrypt the inventory results.
newtype InventoryEncryption = InventoryEncryption 
  { "SSES3" :: NullOrUndefined (SSES3)
  , "SSEKMS" :: NullOrUndefined (SSEKMS)
  }
derive instance newtypeInventoryEncryption :: Newtype InventoryEncryption _
derive instance repGenericInventoryEncryption :: Generic InventoryEncryption _
instance showInventoryEncryption :: Show InventoryEncryption where show = genericShow
instance decodeInventoryEncryption :: Decode InventoryEncryption where decode = genericDecode options
instance encodeInventoryEncryption :: Encode InventoryEncryption where encode = genericEncode options

-- | Constructs InventoryEncryption from required parameters
newInventoryEncryption :: InventoryEncryption
newInventoryEncryption  = InventoryEncryption { "SSEKMS": (NullOrUndefined Nothing), "SSES3": (NullOrUndefined Nothing) }

-- | Constructs InventoryEncryption's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newInventoryEncryption' :: ( { "SSES3" :: NullOrUndefined (SSES3) , "SSEKMS" :: NullOrUndefined (SSEKMS) } -> {"SSES3" :: NullOrUndefined (SSES3) , "SSEKMS" :: NullOrUndefined (SSEKMS) } ) -> InventoryEncryption
newInventoryEncryption'  customize = (InventoryEncryption <<< customize) { "SSEKMS": (NullOrUndefined Nothing), "SSES3": (NullOrUndefined Nothing) }



newtype InventoryFilter = InventoryFilter 
  { "Prefix" :: (Prefix)
  }
derive instance newtypeInventoryFilter :: Newtype InventoryFilter _
derive instance repGenericInventoryFilter :: Generic InventoryFilter _
instance showInventoryFilter :: Show InventoryFilter where show = genericShow
instance decodeInventoryFilter :: Decode InventoryFilter where decode = genericDecode options
instance encodeInventoryFilter :: Encode InventoryFilter where encode = genericEncode options

-- | Constructs InventoryFilter from required parameters
newInventoryFilter :: Prefix -> InventoryFilter
newInventoryFilter _Prefix = InventoryFilter { "Prefix": _Prefix }

-- | Constructs InventoryFilter's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newInventoryFilter' :: Prefix -> ( { "Prefix" :: (Prefix) } -> {"Prefix" :: (Prefix) } ) -> InventoryFilter
newInventoryFilter' _Prefix customize = (InventoryFilter <<< customize) { "Prefix": _Prefix }



newtype InventoryFormat = InventoryFormat String
derive instance newtypeInventoryFormat :: Newtype InventoryFormat _
derive instance repGenericInventoryFormat :: Generic InventoryFormat _
instance showInventoryFormat :: Show InventoryFormat where show = genericShow
instance decodeInventoryFormat :: Decode InventoryFormat where decode = genericDecode options
instance encodeInventoryFormat :: Encode InventoryFormat where encode = genericEncode options



newtype InventoryFrequency = InventoryFrequency String
derive instance newtypeInventoryFrequency :: Newtype InventoryFrequency _
derive instance repGenericInventoryFrequency :: Generic InventoryFrequency _
instance showInventoryFrequency :: Show InventoryFrequency where show = genericShow
instance decodeInventoryFrequency :: Decode InventoryFrequency where decode = genericDecode options
instance encodeInventoryFrequency :: Encode InventoryFrequency where encode = genericEncode options



newtype InventoryId = InventoryId String
derive instance newtypeInventoryId :: Newtype InventoryId _
derive instance repGenericInventoryId :: Generic InventoryId _
instance showInventoryId :: Show InventoryId where show = genericShow
instance decodeInventoryId :: Decode InventoryId where decode = genericDecode options
instance encodeInventoryId :: Encode InventoryId where encode = genericEncode options



newtype InventoryIncludedObjectVersions = InventoryIncludedObjectVersions String
derive instance newtypeInventoryIncludedObjectVersions :: Newtype InventoryIncludedObjectVersions _
derive instance repGenericInventoryIncludedObjectVersions :: Generic InventoryIncludedObjectVersions _
instance showInventoryIncludedObjectVersions :: Show InventoryIncludedObjectVersions where show = genericShow
instance decodeInventoryIncludedObjectVersions :: Decode InventoryIncludedObjectVersions where decode = genericDecode options
instance encodeInventoryIncludedObjectVersions :: Encode InventoryIncludedObjectVersions where encode = genericEncode options



newtype InventoryOptionalField = InventoryOptionalField String
derive instance newtypeInventoryOptionalField :: Newtype InventoryOptionalField _
derive instance repGenericInventoryOptionalField :: Generic InventoryOptionalField _
instance showInventoryOptionalField :: Show InventoryOptionalField where show = genericShow
instance decodeInventoryOptionalField :: Decode InventoryOptionalField where decode = genericDecode options
instance encodeInventoryOptionalField :: Encode InventoryOptionalField where encode = genericEncode options



newtype InventoryOptionalFields = InventoryOptionalFields (Array InventoryOptionalField)
derive instance newtypeInventoryOptionalFields :: Newtype InventoryOptionalFields _
derive instance repGenericInventoryOptionalFields :: Generic InventoryOptionalFields _
instance showInventoryOptionalFields :: Show InventoryOptionalFields where show = genericShow
instance decodeInventoryOptionalFields :: Decode InventoryOptionalFields where decode = genericDecode options
instance encodeInventoryOptionalFields :: Encode InventoryOptionalFields where encode = genericEncode options



newtype InventoryS3BucketDestination = InventoryS3BucketDestination 
  { "AccountId" :: NullOrUndefined (AccountId)
  , "Bucket" :: (BucketName)
  , "Format" :: (InventoryFormat)
  , "Prefix" :: NullOrUndefined (Prefix)
  , "Encryption" :: NullOrUndefined (InventoryEncryption)
  }
derive instance newtypeInventoryS3BucketDestination :: Newtype InventoryS3BucketDestination _
derive instance repGenericInventoryS3BucketDestination :: Generic InventoryS3BucketDestination _
instance showInventoryS3BucketDestination :: Show InventoryS3BucketDestination where show = genericShow
instance decodeInventoryS3BucketDestination :: Decode InventoryS3BucketDestination where decode = genericDecode options
instance encodeInventoryS3BucketDestination :: Encode InventoryS3BucketDestination where encode = genericEncode options

-- | Constructs InventoryS3BucketDestination from required parameters
newInventoryS3BucketDestination :: BucketName -> InventoryFormat -> InventoryS3BucketDestination
newInventoryS3BucketDestination _Bucket _Format = InventoryS3BucketDestination { "Bucket": _Bucket, "Format": _Format, "AccountId": (NullOrUndefined Nothing), "Encryption": (NullOrUndefined Nothing), "Prefix": (NullOrUndefined Nothing) }

-- | Constructs InventoryS3BucketDestination's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newInventoryS3BucketDestination' :: BucketName -> InventoryFormat -> ( { "AccountId" :: NullOrUndefined (AccountId) , "Bucket" :: (BucketName) , "Format" :: (InventoryFormat) , "Prefix" :: NullOrUndefined (Prefix) , "Encryption" :: NullOrUndefined (InventoryEncryption) } -> {"AccountId" :: NullOrUndefined (AccountId) , "Bucket" :: (BucketName) , "Format" :: (InventoryFormat) , "Prefix" :: NullOrUndefined (Prefix) , "Encryption" :: NullOrUndefined (InventoryEncryption) } ) -> InventoryS3BucketDestination
newInventoryS3BucketDestination' _Bucket _Format customize = (InventoryS3BucketDestination <<< customize) { "Bucket": _Bucket, "Format": _Format, "AccountId": (NullOrUndefined Nothing), "Encryption": (NullOrUndefined Nothing), "Prefix": (NullOrUndefined Nothing) }



newtype InventorySchedule = InventorySchedule 
  { "Frequency" :: (InventoryFrequency)
  }
derive instance newtypeInventorySchedule :: Newtype InventorySchedule _
derive instance repGenericInventorySchedule :: Generic InventorySchedule _
instance showInventorySchedule :: Show InventorySchedule where show = genericShow
instance decodeInventorySchedule :: Decode InventorySchedule where decode = genericDecode options
instance encodeInventorySchedule :: Encode InventorySchedule where encode = genericEncode options

-- | Constructs InventorySchedule from required parameters
newInventorySchedule :: InventoryFrequency -> InventorySchedule
newInventorySchedule _Frequency = InventorySchedule { "Frequency": _Frequency }

-- | Constructs InventorySchedule's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newInventorySchedule' :: InventoryFrequency -> ( { "Frequency" :: (InventoryFrequency) } -> {"Frequency" :: (InventoryFrequency) } ) -> InventorySchedule
newInventorySchedule' _Frequency customize = (InventorySchedule <<< customize) { "Frequency": _Frequency }



newtype IsEnabled = IsEnabled Boolean
derive instance newtypeIsEnabled :: Newtype IsEnabled _
derive instance repGenericIsEnabled :: Generic IsEnabled _
instance showIsEnabled :: Show IsEnabled where show = genericShow
instance decodeIsEnabled :: Decode IsEnabled where decode = genericDecode options
instance encodeIsEnabled :: Encode IsEnabled where encode = genericEncode options



newtype IsLatest = IsLatest Boolean
derive instance newtypeIsLatest :: Newtype IsLatest _
derive instance repGenericIsLatest :: Generic IsLatest _
instance showIsLatest :: Show IsLatest where show = genericShow
instance decodeIsLatest :: Decode IsLatest where decode = genericDecode options
instance encodeIsLatest :: Encode IsLatest where encode = genericEncode options



newtype IsTruncated = IsTruncated Boolean
derive instance newtypeIsTruncated :: Newtype IsTruncated _
derive instance repGenericIsTruncated :: Generic IsTruncated _
instance showIsTruncated :: Show IsTruncated where show = genericShow
instance decodeIsTruncated :: Decode IsTruncated where decode = genericDecode options
instance encodeIsTruncated :: Encode IsTruncated where encode = genericEncode options



newtype KMSContext = KMSContext String
derive instance newtypeKMSContext :: Newtype KMSContext _
derive instance repGenericKMSContext :: Generic KMSContext _
instance showKMSContext :: Show KMSContext where show = genericShow
instance decodeKMSContext :: Decode KMSContext where decode = genericDecode options
instance encodeKMSContext :: Encode KMSContext where encode = genericEncode options



newtype KeyCount = KeyCount Int
derive instance newtypeKeyCount :: Newtype KeyCount _
derive instance repGenericKeyCount :: Generic KeyCount _
instance showKeyCount :: Show KeyCount where show = genericShow
instance decodeKeyCount :: Decode KeyCount where decode = genericDecode options
instance encodeKeyCount :: Encode KeyCount where encode = genericEncode options



newtype KeyMarker = KeyMarker String
derive instance newtypeKeyMarker :: Newtype KeyMarker _
derive instance repGenericKeyMarker :: Generic KeyMarker _
instance showKeyMarker :: Show KeyMarker where show = genericShow
instance decodeKeyMarker :: Decode KeyMarker where decode = genericDecode options
instance encodeKeyMarker :: Encode KeyMarker where encode = genericEncode options



newtype KeyPrefixEquals = KeyPrefixEquals String
derive instance newtypeKeyPrefixEquals :: Newtype KeyPrefixEquals _
derive instance repGenericKeyPrefixEquals :: Generic KeyPrefixEquals _
instance showKeyPrefixEquals :: Show KeyPrefixEquals where show = genericShow
instance decodeKeyPrefixEquals :: Decode KeyPrefixEquals where decode = genericDecode options
instance encodeKeyPrefixEquals :: Encode KeyPrefixEquals where encode = genericEncode options



newtype LambdaFunctionArn = LambdaFunctionArn String
derive instance newtypeLambdaFunctionArn :: Newtype LambdaFunctionArn _
derive instance repGenericLambdaFunctionArn :: Generic LambdaFunctionArn _
instance showLambdaFunctionArn :: Show LambdaFunctionArn where show = genericShow
instance decodeLambdaFunctionArn :: Decode LambdaFunctionArn where decode = genericDecode options
instance encodeLambdaFunctionArn :: Encode LambdaFunctionArn where encode = genericEncode options



-- | Container for specifying the AWS Lambda notification configuration.
newtype LambdaFunctionConfiguration = LambdaFunctionConfiguration 
  { "Id" :: NullOrUndefined (NotificationId)
  , "LambdaFunctionArn" :: (LambdaFunctionArn)
  , "Events" :: (EventList)
  , "Filter" :: NullOrUndefined (NotificationConfigurationFilter)
  }
derive instance newtypeLambdaFunctionConfiguration :: Newtype LambdaFunctionConfiguration _
derive instance repGenericLambdaFunctionConfiguration :: Generic LambdaFunctionConfiguration _
instance showLambdaFunctionConfiguration :: Show LambdaFunctionConfiguration where show = genericShow
instance decodeLambdaFunctionConfiguration :: Decode LambdaFunctionConfiguration where decode = genericDecode options
instance encodeLambdaFunctionConfiguration :: Encode LambdaFunctionConfiguration where encode = genericEncode options

-- | Constructs LambdaFunctionConfiguration from required parameters
newLambdaFunctionConfiguration :: EventList -> LambdaFunctionArn -> LambdaFunctionConfiguration
newLambdaFunctionConfiguration _Events _LambdaFunctionArn = LambdaFunctionConfiguration { "Events": _Events, "LambdaFunctionArn": _LambdaFunctionArn, "Filter": (NullOrUndefined Nothing), "Id": (NullOrUndefined Nothing) }

-- | Constructs LambdaFunctionConfiguration's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newLambdaFunctionConfiguration' :: EventList -> LambdaFunctionArn -> ( { "Id" :: NullOrUndefined (NotificationId) , "LambdaFunctionArn" :: (LambdaFunctionArn) , "Events" :: (EventList) , "Filter" :: NullOrUndefined (NotificationConfigurationFilter) } -> {"Id" :: NullOrUndefined (NotificationId) , "LambdaFunctionArn" :: (LambdaFunctionArn) , "Events" :: (EventList) , "Filter" :: NullOrUndefined (NotificationConfigurationFilter) } ) -> LambdaFunctionConfiguration
newLambdaFunctionConfiguration' _Events _LambdaFunctionArn customize = (LambdaFunctionConfiguration <<< customize) { "Events": _Events, "LambdaFunctionArn": _LambdaFunctionArn, "Filter": (NullOrUndefined Nothing), "Id": (NullOrUndefined Nothing) }



newtype LambdaFunctionConfigurationList = LambdaFunctionConfigurationList (Array LambdaFunctionConfiguration)
derive instance newtypeLambdaFunctionConfigurationList :: Newtype LambdaFunctionConfigurationList _
derive instance repGenericLambdaFunctionConfigurationList :: Generic LambdaFunctionConfigurationList _
instance showLambdaFunctionConfigurationList :: Show LambdaFunctionConfigurationList where show = genericShow
instance decodeLambdaFunctionConfigurationList :: Decode LambdaFunctionConfigurationList where decode = genericDecode options
instance encodeLambdaFunctionConfigurationList :: Encode LambdaFunctionConfigurationList where encode = genericEncode options



newtype LastModified = LastModified Types.Timestamp
derive instance newtypeLastModified :: Newtype LastModified _
derive instance repGenericLastModified :: Generic LastModified _
instance showLastModified :: Show LastModified where show = genericShow
instance decodeLastModified :: Decode LastModified where decode = genericDecode options
instance encodeLastModified :: Encode LastModified where encode = genericEncode options



newtype LifecycleConfiguration = LifecycleConfiguration 
  { "Rules" :: (Rules)
  }
derive instance newtypeLifecycleConfiguration :: Newtype LifecycleConfiguration _
derive instance repGenericLifecycleConfiguration :: Generic LifecycleConfiguration _
instance showLifecycleConfiguration :: Show LifecycleConfiguration where show = genericShow
instance decodeLifecycleConfiguration :: Decode LifecycleConfiguration where decode = genericDecode options
instance encodeLifecycleConfiguration :: Encode LifecycleConfiguration where encode = genericEncode options

-- | Constructs LifecycleConfiguration from required parameters
newLifecycleConfiguration :: Rules -> LifecycleConfiguration
newLifecycleConfiguration _Rules = LifecycleConfiguration { "Rules": _Rules }

-- | Constructs LifecycleConfiguration's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newLifecycleConfiguration' :: Rules -> ( { "Rules" :: (Rules) } -> {"Rules" :: (Rules) } ) -> LifecycleConfiguration
newLifecycleConfiguration' _Rules customize = (LifecycleConfiguration <<< customize) { "Rules": _Rules }



newtype LifecycleExpiration = LifecycleExpiration 
  { "Date" :: NullOrUndefined (Date)
  , "Days" :: NullOrUndefined (Days)
  , "ExpiredObjectDeleteMarker" :: NullOrUndefined (ExpiredObjectDeleteMarker)
  }
derive instance newtypeLifecycleExpiration :: Newtype LifecycleExpiration _
derive instance repGenericLifecycleExpiration :: Generic LifecycleExpiration _
instance showLifecycleExpiration :: Show LifecycleExpiration where show = genericShow
instance decodeLifecycleExpiration :: Decode LifecycleExpiration where decode = genericDecode options
instance encodeLifecycleExpiration :: Encode LifecycleExpiration where encode = genericEncode options

-- | Constructs LifecycleExpiration from required parameters
newLifecycleExpiration :: LifecycleExpiration
newLifecycleExpiration  = LifecycleExpiration { "Date": (NullOrUndefined Nothing), "Days": (NullOrUndefined Nothing), "ExpiredObjectDeleteMarker": (NullOrUndefined Nothing) }

-- | Constructs LifecycleExpiration's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newLifecycleExpiration' :: ( { "Date" :: NullOrUndefined (Date) , "Days" :: NullOrUndefined (Days) , "ExpiredObjectDeleteMarker" :: NullOrUndefined (ExpiredObjectDeleteMarker) } -> {"Date" :: NullOrUndefined (Date) , "Days" :: NullOrUndefined (Days) , "ExpiredObjectDeleteMarker" :: NullOrUndefined (ExpiredObjectDeleteMarker) } ) -> LifecycleExpiration
newLifecycleExpiration'  customize = (LifecycleExpiration <<< customize) { "Date": (NullOrUndefined Nothing), "Days": (NullOrUndefined Nothing), "ExpiredObjectDeleteMarker": (NullOrUndefined Nothing) }



newtype LifecycleRule = LifecycleRule 
  { "Expiration" :: NullOrUndefined (LifecycleExpiration)
  , "ID" :: NullOrUndefined (ID)
  , "Prefix" :: NullOrUndefined (Prefix)
  , "Filter" :: NullOrUndefined (LifecycleRuleFilter)
  , "Status" :: (ExpirationStatus)
  , "Transitions" :: NullOrUndefined (TransitionList)
  , "NoncurrentVersionTransitions" :: NullOrUndefined (NoncurrentVersionTransitionList)
  , "NoncurrentVersionExpiration" :: NullOrUndefined (NoncurrentVersionExpiration)
  , "AbortIncompleteMultipartUpload" :: NullOrUndefined (AbortIncompleteMultipartUpload)
  }
derive instance newtypeLifecycleRule :: Newtype LifecycleRule _
derive instance repGenericLifecycleRule :: Generic LifecycleRule _
instance showLifecycleRule :: Show LifecycleRule where show = genericShow
instance decodeLifecycleRule :: Decode LifecycleRule where decode = genericDecode options
instance encodeLifecycleRule :: Encode LifecycleRule where encode = genericEncode options

-- | Constructs LifecycleRule from required parameters
newLifecycleRule :: ExpirationStatus -> LifecycleRule
newLifecycleRule _Status = LifecycleRule { "Status": _Status, "AbortIncompleteMultipartUpload": (NullOrUndefined Nothing), "Expiration": (NullOrUndefined Nothing), "Filter": (NullOrUndefined Nothing), "ID": (NullOrUndefined Nothing), "NoncurrentVersionExpiration": (NullOrUndefined Nothing), "NoncurrentVersionTransitions": (NullOrUndefined Nothing), "Prefix": (NullOrUndefined Nothing), "Transitions": (NullOrUndefined Nothing) }

-- | Constructs LifecycleRule's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newLifecycleRule' :: ExpirationStatus -> ( { "Expiration" :: NullOrUndefined (LifecycleExpiration) , "ID" :: NullOrUndefined (ID) , "Prefix" :: NullOrUndefined (Prefix) , "Filter" :: NullOrUndefined (LifecycleRuleFilter) , "Status" :: (ExpirationStatus) , "Transitions" :: NullOrUndefined (TransitionList) , "NoncurrentVersionTransitions" :: NullOrUndefined (NoncurrentVersionTransitionList) , "NoncurrentVersionExpiration" :: NullOrUndefined (NoncurrentVersionExpiration) , "AbortIncompleteMultipartUpload" :: NullOrUndefined (AbortIncompleteMultipartUpload) } -> {"Expiration" :: NullOrUndefined (LifecycleExpiration) , "ID" :: NullOrUndefined (ID) , "Prefix" :: NullOrUndefined (Prefix) , "Filter" :: NullOrUndefined (LifecycleRuleFilter) , "Status" :: (ExpirationStatus) , "Transitions" :: NullOrUndefined (TransitionList) , "NoncurrentVersionTransitions" :: NullOrUndefined (NoncurrentVersionTransitionList) , "NoncurrentVersionExpiration" :: NullOrUndefined (NoncurrentVersionExpiration) , "AbortIncompleteMultipartUpload" :: NullOrUndefined (AbortIncompleteMultipartUpload) } ) -> LifecycleRule
newLifecycleRule' _Status customize = (LifecycleRule <<< customize) { "Status": _Status, "AbortIncompleteMultipartUpload": (NullOrUndefined Nothing), "Expiration": (NullOrUndefined Nothing), "Filter": (NullOrUndefined Nothing), "ID": (NullOrUndefined Nothing), "NoncurrentVersionExpiration": (NullOrUndefined Nothing), "NoncurrentVersionTransitions": (NullOrUndefined Nothing), "Prefix": (NullOrUndefined Nothing), "Transitions": (NullOrUndefined Nothing) }



-- | This is used in a Lifecycle Rule Filter to apply a logical AND to two or more predicates. The Lifecycle Rule will apply to any object matching all of the predicates configured inside the And operator.
newtype LifecycleRuleAndOperator = LifecycleRuleAndOperator 
  { "Prefix" :: NullOrUndefined (Prefix)
  , "Tags" :: NullOrUndefined (TagSet)
  }
derive instance newtypeLifecycleRuleAndOperator :: Newtype LifecycleRuleAndOperator _
derive instance repGenericLifecycleRuleAndOperator :: Generic LifecycleRuleAndOperator _
instance showLifecycleRuleAndOperator :: Show LifecycleRuleAndOperator where show = genericShow
instance decodeLifecycleRuleAndOperator :: Decode LifecycleRuleAndOperator where decode = genericDecode options
instance encodeLifecycleRuleAndOperator :: Encode LifecycleRuleAndOperator where encode = genericEncode options

-- | Constructs LifecycleRuleAndOperator from required parameters
newLifecycleRuleAndOperator :: LifecycleRuleAndOperator
newLifecycleRuleAndOperator  = LifecycleRuleAndOperator { "Prefix": (NullOrUndefined Nothing), "Tags": (NullOrUndefined Nothing) }

-- | Constructs LifecycleRuleAndOperator's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newLifecycleRuleAndOperator' :: ( { "Prefix" :: NullOrUndefined (Prefix) , "Tags" :: NullOrUndefined (TagSet) } -> {"Prefix" :: NullOrUndefined (Prefix) , "Tags" :: NullOrUndefined (TagSet) } ) -> LifecycleRuleAndOperator
newLifecycleRuleAndOperator'  customize = (LifecycleRuleAndOperator <<< customize) { "Prefix": (NullOrUndefined Nothing), "Tags": (NullOrUndefined Nothing) }



-- | The Filter is used to identify objects that a Lifecycle Rule applies to. A Filter must have exactly one of Prefix, Tag, or And specified.
newtype LifecycleRuleFilter = LifecycleRuleFilter 
  { "Prefix" :: NullOrUndefined (Prefix)
  , "Tag" :: NullOrUndefined (Tag)
  , "And" :: NullOrUndefined (LifecycleRuleAndOperator)
  }
derive instance newtypeLifecycleRuleFilter :: Newtype LifecycleRuleFilter _
derive instance repGenericLifecycleRuleFilter :: Generic LifecycleRuleFilter _
instance showLifecycleRuleFilter :: Show LifecycleRuleFilter where show = genericShow
instance decodeLifecycleRuleFilter :: Decode LifecycleRuleFilter where decode = genericDecode options
instance encodeLifecycleRuleFilter :: Encode LifecycleRuleFilter where encode = genericEncode options

-- | Constructs LifecycleRuleFilter from required parameters
newLifecycleRuleFilter :: LifecycleRuleFilter
newLifecycleRuleFilter  = LifecycleRuleFilter { "And": (NullOrUndefined Nothing), "Prefix": (NullOrUndefined Nothing), "Tag": (NullOrUndefined Nothing) }

-- | Constructs LifecycleRuleFilter's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newLifecycleRuleFilter' :: ( { "Prefix" :: NullOrUndefined (Prefix) , "Tag" :: NullOrUndefined (Tag) , "And" :: NullOrUndefined (LifecycleRuleAndOperator) } -> {"Prefix" :: NullOrUndefined (Prefix) , "Tag" :: NullOrUndefined (Tag) , "And" :: NullOrUndefined (LifecycleRuleAndOperator) } ) -> LifecycleRuleFilter
newLifecycleRuleFilter'  customize = (LifecycleRuleFilter <<< customize) { "And": (NullOrUndefined Nothing), "Prefix": (NullOrUndefined Nothing), "Tag": (NullOrUndefined Nothing) }



newtype LifecycleRules = LifecycleRules (Array LifecycleRule)
derive instance newtypeLifecycleRules :: Newtype LifecycleRules _
derive instance repGenericLifecycleRules :: Generic LifecycleRules _
instance showLifecycleRules :: Show LifecycleRules where show = genericShow
instance decodeLifecycleRules :: Decode LifecycleRules where decode = genericDecode options
instance encodeLifecycleRules :: Encode LifecycleRules where encode = genericEncode options



newtype ListBucketAnalyticsConfigurationsOutput = ListBucketAnalyticsConfigurationsOutput 
  { "IsTruncated" :: NullOrUndefined (IsTruncated)
  , "ContinuationToken" :: NullOrUndefined (Token)
  , "NextContinuationToken" :: NullOrUndefined (NextToken)
  , "AnalyticsConfigurationList" :: NullOrUndefined (AnalyticsConfigurationList)
  }
derive instance newtypeListBucketAnalyticsConfigurationsOutput :: Newtype ListBucketAnalyticsConfigurationsOutput _
derive instance repGenericListBucketAnalyticsConfigurationsOutput :: Generic ListBucketAnalyticsConfigurationsOutput _
instance showListBucketAnalyticsConfigurationsOutput :: Show ListBucketAnalyticsConfigurationsOutput where show = genericShow
instance decodeListBucketAnalyticsConfigurationsOutput :: Decode ListBucketAnalyticsConfigurationsOutput where decode = genericDecode options
instance encodeListBucketAnalyticsConfigurationsOutput :: Encode ListBucketAnalyticsConfigurationsOutput where encode = genericEncode options

-- | Constructs ListBucketAnalyticsConfigurationsOutput from required parameters
newListBucketAnalyticsConfigurationsOutput :: ListBucketAnalyticsConfigurationsOutput
newListBucketAnalyticsConfigurationsOutput  = ListBucketAnalyticsConfigurationsOutput { "AnalyticsConfigurationList": (NullOrUndefined Nothing), "ContinuationToken": (NullOrUndefined Nothing), "IsTruncated": (NullOrUndefined Nothing), "NextContinuationToken": (NullOrUndefined Nothing) }

-- | Constructs ListBucketAnalyticsConfigurationsOutput's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newListBucketAnalyticsConfigurationsOutput' :: ( { "IsTruncated" :: NullOrUndefined (IsTruncated) , "ContinuationToken" :: NullOrUndefined (Token) , "NextContinuationToken" :: NullOrUndefined (NextToken) , "AnalyticsConfigurationList" :: NullOrUndefined (AnalyticsConfigurationList) } -> {"IsTruncated" :: NullOrUndefined (IsTruncated) , "ContinuationToken" :: NullOrUndefined (Token) , "NextContinuationToken" :: NullOrUndefined (NextToken) , "AnalyticsConfigurationList" :: NullOrUndefined (AnalyticsConfigurationList) } ) -> ListBucketAnalyticsConfigurationsOutput
newListBucketAnalyticsConfigurationsOutput'  customize = (ListBucketAnalyticsConfigurationsOutput <<< customize) { "AnalyticsConfigurationList": (NullOrUndefined Nothing), "ContinuationToken": (NullOrUndefined Nothing), "IsTruncated": (NullOrUndefined Nothing), "NextContinuationToken": (NullOrUndefined Nothing) }



newtype ListBucketAnalyticsConfigurationsRequest = ListBucketAnalyticsConfigurationsRequest 
  { "Bucket" :: (BucketName)
  , "ContinuationToken" :: NullOrUndefined (Token)
  }
derive instance newtypeListBucketAnalyticsConfigurationsRequest :: Newtype ListBucketAnalyticsConfigurationsRequest _
derive instance repGenericListBucketAnalyticsConfigurationsRequest :: Generic ListBucketAnalyticsConfigurationsRequest _
instance showListBucketAnalyticsConfigurationsRequest :: Show ListBucketAnalyticsConfigurationsRequest where show = genericShow
instance decodeListBucketAnalyticsConfigurationsRequest :: Decode ListBucketAnalyticsConfigurationsRequest where decode = genericDecode options
instance encodeListBucketAnalyticsConfigurationsRequest :: Encode ListBucketAnalyticsConfigurationsRequest where encode = genericEncode options

-- | Constructs ListBucketAnalyticsConfigurationsRequest from required parameters
newListBucketAnalyticsConfigurationsRequest :: BucketName -> ListBucketAnalyticsConfigurationsRequest
newListBucketAnalyticsConfigurationsRequest _Bucket = ListBucketAnalyticsConfigurationsRequest { "Bucket": _Bucket, "ContinuationToken": (NullOrUndefined Nothing) }

-- | Constructs ListBucketAnalyticsConfigurationsRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newListBucketAnalyticsConfigurationsRequest' :: BucketName -> ( { "Bucket" :: (BucketName) , "ContinuationToken" :: NullOrUndefined (Token) } -> {"Bucket" :: (BucketName) , "ContinuationToken" :: NullOrUndefined (Token) } ) -> ListBucketAnalyticsConfigurationsRequest
newListBucketAnalyticsConfigurationsRequest' _Bucket customize = (ListBucketAnalyticsConfigurationsRequest <<< customize) { "Bucket": _Bucket, "ContinuationToken": (NullOrUndefined Nothing) }



newtype ListBucketInventoryConfigurationsOutput = ListBucketInventoryConfigurationsOutput 
  { "ContinuationToken" :: NullOrUndefined (Token)
  , "InventoryConfigurationList" :: NullOrUndefined (InventoryConfigurationList)
  , "IsTruncated" :: NullOrUndefined (IsTruncated)
  , "NextContinuationToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListBucketInventoryConfigurationsOutput :: Newtype ListBucketInventoryConfigurationsOutput _
derive instance repGenericListBucketInventoryConfigurationsOutput :: Generic ListBucketInventoryConfigurationsOutput _
instance showListBucketInventoryConfigurationsOutput :: Show ListBucketInventoryConfigurationsOutput where show = genericShow
instance decodeListBucketInventoryConfigurationsOutput :: Decode ListBucketInventoryConfigurationsOutput where decode = genericDecode options
instance encodeListBucketInventoryConfigurationsOutput :: Encode ListBucketInventoryConfigurationsOutput where encode = genericEncode options

-- | Constructs ListBucketInventoryConfigurationsOutput from required parameters
newListBucketInventoryConfigurationsOutput :: ListBucketInventoryConfigurationsOutput
newListBucketInventoryConfigurationsOutput  = ListBucketInventoryConfigurationsOutput { "ContinuationToken": (NullOrUndefined Nothing), "InventoryConfigurationList": (NullOrUndefined Nothing), "IsTruncated": (NullOrUndefined Nothing), "NextContinuationToken": (NullOrUndefined Nothing) }

-- | Constructs ListBucketInventoryConfigurationsOutput's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newListBucketInventoryConfigurationsOutput' :: ( { "ContinuationToken" :: NullOrUndefined (Token) , "InventoryConfigurationList" :: NullOrUndefined (InventoryConfigurationList) , "IsTruncated" :: NullOrUndefined (IsTruncated) , "NextContinuationToken" :: NullOrUndefined (NextToken) } -> {"ContinuationToken" :: NullOrUndefined (Token) , "InventoryConfigurationList" :: NullOrUndefined (InventoryConfigurationList) , "IsTruncated" :: NullOrUndefined (IsTruncated) , "NextContinuationToken" :: NullOrUndefined (NextToken) } ) -> ListBucketInventoryConfigurationsOutput
newListBucketInventoryConfigurationsOutput'  customize = (ListBucketInventoryConfigurationsOutput <<< customize) { "ContinuationToken": (NullOrUndefined Nothing), "InventoryConfigurationList": (NullOrUndefined Nothing), "IsTruncated": (NullOrUndefined Nothing), "NextContinuationToken": (NullOrUndefined Nothing) }



newtype ListBucketInventoryConfigurationsRequest = ListBucketInventoryConfigurationsRequest 
  { "Bucket" :: (BucketName)
  , "ContinuationToken" :: NullOrUndefined (Token)
  }
derive instance newtypeListBucketInventoryConfigurationsRequest :: Newtype ListBucketInventoryConfigurationsRequest _
derive instance repGenericListBucketInventoryConfigurationsRequest :: Generic ListBucketInventoryConfigurationsRequest _
instance showListBucketInventoryConfigurationsRequest :: Show ListBucketInventoryConfigurationsRequest where show = genericShow
instance decodeListBucketInventoryConfigurationsRequest :: Decode ListBucketInventoryConfigurationsRequest where decode = genericDecode options
instance encodeListBucketInventoryConfigurationsRequest :: Encode ListBucketInventoryConfigurationsRequest where encode = genericEncode options

-- | Constructs ListBucketInventoryConfigurationsRequest from required parameters
newListBucketInventoryConfigurationsRequest :: BucketName -> ListBucketInventoryConfigurationsRequest
newListBucketInventoryConfigurationsRequest _Bucket = ListBucketInventoryConfigurationsRequest { "Bucket": _Bucket, "ContinuationToken": (NullOrUndefined Nothing) }

-- | Constructs ListBucketInventoryConfigurationsRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newListBucketInventoryConfigurationsRequest' :: BucketName -> ( { "Bucket" :: (BucketName) , "ContinuationToken" :: NullOrUndefined (Token) } -> {"Bucket" :: (BucketName) , "ContinuationToken" :: NullOrUndefined (Token) } ) -> ListBucketInventoryConfigurationsRequest
newListBucketInventoryConfigurationsRequest' _Bucket customize = (ListBucketInventoryConfigurationsRequest <<< customize) { "Bucket": _Bucket, "ContinuationToken": (NullOrUndefined Nothing) }



newtype ListBucketMetricsConfigurationsOutput = ListBucketMetricsConfigurationsOutput 
  { "IsTruncated" :: NullOrUndefined (IsTruncated)
  , "ContinuationToken" :: NullOrUndefined (Token)
  , "NextContinuationToken" :: NullOrUndefined (NextToken)
  , "MetricsConfigurationList" :: NullOrUndefined (MetricsConfigurationList)
  }
derive instance newtypeListBucketMetricsConfigurationsOutput :: Newtype ListBucketMetricsConfigurationsOutput _
derive instance repGenericListBucketMetricsConfigurationsOutput :: Generic ListBucketMetricsConfigurationsOutput _
instance showListBucketMetricsConfigurationsOutput :: Show ListBucketMetricsConfigurationsOutput where show = genericShow
instance decodeListBucketMetricsConfigurationsOutput :: Decode ListBucketMetricsConfigurationsOutput where decode = genericDecode options
instance encodeListBucketMetricsConfigurationsOutput :: Encode ListBucketMetricsConfigurationsOutput where encode = genericEncode options

-- | Constructs ListBucketMetricsConfigurationsOutput from required parameters
newListBucketMetricsConfigurationsOutput :: ListBucketMetricsConfigurationsOutput
newListBucketMetricsConfigurationsOutput  = ListBucketMetricsConfigurationsOutput { "ContinuationToken": (NullOrUndefined Nothing), "IsTruncated": (NullOrUndefined Nothing), "MetricsConfigurationList": (NullOrUndefined Nothing), "NextContinuationToken": (NullOrUndefined Nothing) }

-- | Constructs ListBucketMetricsConfigurationsOutput's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newListBucketMetricsConfigurationsOutput' :: ( { "IsTruncated" :: NullOrUndefined (IsTruncated) , "ContinuationToken" :: NullOrUndefined (Token) , "NextContinuationToken" :: NullOrUndefined (NextToken) , "MetricsConfigurationList" :: NullOrUndefined (MetricsConfigurationList) } -> {"IsTruncated" :: NullOrUndefined (IsTruncated) , "ContinuationToken" :: NullOrUndefined (Token) , "NextContinuationToken" :: NullOrUndefined (NextToken) , "MetricsConfigurationList" :: NullOrUndefined (MetricsConfigurationList) } ) -> ListBucketMetricsConfigurationsOutput
newListBucketMetricsConfigurationsOutput'  customize = (ListBucketMetricsConfigurationsOutput <<< customize) { "ContinuationToken": (NullOrUndefined Nothing), "IsTruncated": (NullOrUndefined Nothing), "MetricsConfigurationList": (NullOrUndefined Nothing), "NextContinuationToken": (NullOrUndefined Nothing) }



newtype ListBucketMetricsConfigurationsRequest = ListBucketMetricsConfigurationsRequest 
  { "Bucket" :: (BucketName)
  , "ContinuationToken" :: NullOrUndefined (Token)
  }
derive instance newtypeListBucketMetricsConfigurationsRequest :: Newtype ListBucketMetricsConfigurationsRequest _
derive instance repGenericListBucketMetricsConfigurationsRequest :: Generic ListBucketMetricsConfigurationsRequest _
instance showListBucketMetricsConfigurationsRequest :: Show ListBucketMetricsConfigurationsRequest where show = genericShow
instance decodeListBucketMetricsConfigurationsRequest :: Decode ListBucketMetricsConfigurationsRequest where decode = genericDecode options
instance encodeListBucketMetricsConfigurationsRequest :: Encode ListBucketMetricsConfigurationsRequest where encode = genericEncode options

-- | Constructs ListBucketMetricsConfigurationsRequest from required parameters
newListBucketMetricsConfigurationsRequest :: BucketName -> ListBucketMetricsConfigurationsRequest
newListBucketMetricsConfigurationsRequest _Bucket = ListBucketMetricsConfigurationsRequest { "Bucket": _Bucket, "ContinuationToken": (NullOrUndefined Nothing) }

-- | Constructs ListBucketMetricsConfigurationsRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newListBucketMetricsConfigurationsRequest' :: BucketName -> ( { "Bucket" :: (BucketName) , "ContinuationToken" :: NullOrUndefined (Token) } -> {"Bucket" :: (BucketName) , "ContinuationToken" :: NullOrUndefined (Token) } ) -> ListBucketMetricsConfigurationsRequest
newListBucketMetricsConfigurationsRequest' _Bucket customize = (ListBucketMetricsConfigurationsRequest <<< customize) { "Bucket": _Bucket, "ContinuationToken": (NullOrUndefined Nothing) }



newtype ListBucketsOutput = ListBucketsOutput 
  { "Buckets" :: NullOrUndefined (Buckets)
  , "Owner" :: NullOrUndefined (Owner)
  }
derive instance newtypeListBucketsOutput :: Newtype ListBucketsOutput _
derive instance repGenericListBucketsOutput :: Generic ListBucketsOutput _
instance showListBucketsOutput :: Show ListBucketsOutput where show = genericShow
instance decodeListBucketsOutput :: Decode ListBucketsOutput where decode = genericDecode options
instance encodeListBucketsOutput :: Encode ListBucketsOutput where encode = genericEncode options

-- | Constructs ListBucketsOutput from required parameters
newListBucketsOutput :: ListBucketsOutput
newListBucketsOutput  = ListBucketsOutput { "Buckets": (NullOrUndefined Nothing), "Owner": (NullOrUndefined Nothing) }

-- | Constructs ListBucketsOutput's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newListBucketsOutput' :: ( { "Buckets" :: NullOrUndefined (Buckets) , "Owner" :: NullOrUndefined (Owner) } -> {"Buckets" :: NullOrUndefined (Buckets) , "Owner" :: NullOrUndefined (Owner) } ) -> ListBucketsOutput
newListBucketsOutput'  customize = (ListBucketsOutput <<< customize) { "Buckets": (NullOrUndefined Nothing), "Owner": (NullOrUndefined Nothing) }



newtype ListMultipartUploadsOutput = ListMultipartUploadsOutput 
  { "Bucket" :: NullOrUndefined (BucketName)
  , "KeyMarker" :: NullOrUndefined (KeyMarker)
  , "UploadIdMarker" :: NullOrUndefined (UploadIdMarker)
  , "NextKeyMarker" :: NullOrUndefined (NextKeyMarker)
  , "Prefix" :: NullOrUndefined (Prefix)
  , "Delimiter" :: NullOrUndefined (Delimiter)
  , "NextUploadIdMarker" :: NullOrUndefined (NextUploadIdMarker)
  , "MaxUploads" :: NullOrUndefined (MaxUploads)
  , "IsTruncated" :: NullOrUndefined (IsTruncated)
  , "Uploads" :: NullOrUndefined (MultipartUploadList)
  , "CommonPrefixes" :: NullOrUndefined (CommonPrefixList)
  , "EncodingType" :: NullOrUndefined (EncodingType)
  }
derive instance newtypeListMultipartUploadsOutput :: Newtype ListMultipartUploadsOutput _
derive instance repGenericListMultipartUploadsOutput :: Generic ListMultipartUploadsOutput _
instance showListMultipartUploadsOutput :: Show ListMultipartUploadsOutput where show = genericShow
instance decodeListMultipartUploadsOutput :: Decode ListMultipartUploadsOutput where decode = genericDecode options
instance encodeListMultipartUploadsOutput :: Encode ListMultipartUploadsOutput where encode = genericEncode options

-- | Constructs ListMultipartUploadsOutput from required parameters
newListMultipartUploadsOutput :: ListMultipartUploadsOutput
newListMultipartUploadsOutput  = ListMultipartUploadsOutput { "Bucket": (NullOrUndefined Nothing), "CommonPrefixes": (NullOrUndefined Nothing), "Delimiter": (NullOrUndefined Nothing), "EncodingType": (NullOrUndefined Nothing), "IsTruncated": (NullOrUndefined Nothing), "KeyMarker": (NullOrUndefined Nothing), "MaxUploads": (NullOrUndefined Nothing), "NextKeyMarker": (NullOrUndefined Nothing), "NextUploadIdMarker": (NullOrUndefined Nothing), "Prefix": (NullOrUndefined Nothing), "UploadIdMarker": (NullOrUndefined Nothing), "Uploads": (NullOrUndefined Nothing) }

-- | Constructs ListMultipartUploadsOutput's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newListMultipartUploadsOutput' :: ( { "Bucket" :: NullOrUndefined (BucketName) , "KeyMarker" :: NullOrUndefined (KeyMarker) , "UploadIdMarker" :: NullOrUndefined (UploadIdMarker) , "NextKeyMarker" :: NullOrUndefined (NextKeyMarker) , "Prefix" :: NullOrUndefined (Prefix) , "Delimiter" :: NullOrUndefined (Delimiter) , "NextUploadIdMarker" :: NullOrUndefined (NextUploadIdMarker) , "MaxUploads" :: NullOrUndefined (MaxUploads) , "IsTruncated" :: NullOrUndefined (IsTruncated) , "Uploads" :: NullOrUndefined (MultipartUploadList) , "CommonPrefixes" :: NullOrUndefined (CommonPrefixList) , "EncodingType" :: NullOrUndefined (EncodingType) } -> {"Bucket" :: NullOrUndefined (BucketName) , "KeyMarker" :: NullOrUndefined (KeyMarker) , "UploadIdMarker" :: NullOrUndefined (UploadIdMarker) , "NextKeyMarker" :: NullOrUndefined (NextKeyMarker) , "Prefix" :: NullOrUndefined (Prefix) , "Delimiter" :: NullOrUndefined (Delimiter) , "NextUploadIdMarker" :: NullOrUndefined (NextUploadIdMarker) , "MaxUploads" :: NullOrUndefined (MaxUploads) , "IsTruncated" :: NullOrUndefined (IsTruncated) , "Uploads" :: NullOrUndefined (MultipartUploadList) , "CommonPrefixes" :: NullOrUndefined (CommonPrefixList) , "EncodingType" :: NullOrUndefined (EncodingType) } ) -> ListMultipartUploadsOutput
newListMultipartUploadsOutput'  customize = (ListMultipartUploadsOutput <<< customize) { "Bucket": (NullOrUndefined Nothing), "CommonPrefixes": (NullOrUndefined Nothing), "Delimiter": (NullOrUndefined Nothing), "EncodingType": (NullOrUndefined Nothing), "IsTruncated": (NullOrUndefined Nothing), "KeyMarker": (NullOrUndefined Nothing), "MaxUploads": (NullOrUndefined Nothing), "NextKeyMarker": (NullOrUndefined Nothing), "NextUploadIdMarker": (NullOrUndefined Nothing), "Prefix": (NullOrUndefined Nothing), "UploadIdMarker": (NullOrUndefined Nothing), "Uploads": (NullOrUndefined Nothing) }



newtype ListMultipartUploadsRequest = ListMultipartUploadsRequest 
  { "Bucket" :: (BucketName)
  , "Delimiter" :: NullOrUndefined (Delimiter)
  , "EncodingType" :: NullOrUndefined (EncodingType)
  , "KeyMarker" :: NullOrUndefined (KeyMarker)
  , "MaxUploads" :: NullOrUndefined (MaxUploads)
  , "Prefix" :: NullOrUndefined (Prefix)
  , "UploadIdMarker" :: NullOrUndefined (UploadIdMarker)
  }
derive instance newtypeListMultipartUploadsRequest :: Newtype ListMultipartUploadsRequest _
derive instance repGenericListMultipartUploadsRequest :: Generic ListMultipartUploadsRequest _
instance showListMultipartUploadsRequest :: Show ListMultipartUploadsRequest where show = genericShow
instance decodeListMultipartUploadsRequest :: Decode ListMultipartUploadsRequest where decode = genericDecode options
instance encodeListMultipartUploadsRequest :: Encode ListMultipartUploadsRequest where encode = genericEncode options

-- | Constructs ListMultipartUploadsRequest from required parameters
newListMultipartUploadsRequest :: BucketName -> ListMultipartUploadsRequest
newListMultipartUploadsRequest _Bucket = ListMultipartUploadsRequest { "Bucket": _Bucket, "Delimiter": (NullOrUndefined Nothing), "EncodingType": (NullOrUndefined Nothing), "KeyMarker": (NullOrUndefined Nothing), "MaxUploads": (NullOrUndefined Nothing), "Prefix": (NullOrUndefined Nothing), "UploadIdMarker": (NullOrUndefined Nothing) }

-- | Constructs ListMultipartUploadsRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newListMultipartUploadsRequest' :: BucketName -> ( { "Bucket" :: (BucketName) , "Delimiter" :: NullOrUndefined (Delimiter) , "EncodingType" :: NullOrUndefined (EncodingType) , "KeyMarker" :: NullOrUndefined (KeyMarker) , "MaxUploads" :: NullOrUndefined (MaxUploads) , "Prefix" :: NullOrUndefined (Prefix) , "UploadIdMarker" :: NullOrUndefined (UploadIdMarker) } -> {"Bucket" :: (BucketName) , "Delimiter" :: NullOrUndefined (Delimiter) , "EncodingType" :: NullOrUndefined (EncodingType) , "KeyMarker" :: NullOrUndefined (KeyMarker) , "MaxUploads" :: NullOrUndefined (MaxUploads) , "Prefix" :: NullOrUndefined (Prefix) , "UploadIdMarker" :: NullOrUndefined (UploadIdMarker) } ) -> ListMultipartUploadsRequest
newListMultipartUploadsRequest' _Bucket customize = (ListMultipartUploadsRequest <<< customize) { "Bucket": _Bucket, "Delimiter": (NullOrUndefined Nothing), "EncodingType": (NullOrUndefined Nothing), "KeyMarker": (NullOrUndefined Nothing), "MaxUploads": (NullOrUndefined Nothing), "Prefix": (NullOrUndefined Nothing), "UploadIdMarker": (NullOrUndefined Nothing) }



newtype ListObjectVersionsOutput = ListObjectVersionsOutput 
  { "IsTruncated" :: NullOrUndefined (IsTruncated)
  , "KeyMarker" :: NullOrUndefined (KeyMarker)
  , "VersionIdMarker" :: NullOrUndefined (VersionIdMarker)
  , "NextKeyMarker" :: NullOrUndefined (NextKeyMarker)
  , "NextVersionIdMarker" :: NullOrUndefined (NextVersionIdMarker)
  , "Versions" :: NullOrUndefined (ObjectVersionList)
  , "DeleteMarkers" :: NullOrUndefined (DeleteMarkers)
  , "Name" :: NullOrUndefined (BucketName)
  , "Prefix" :: NullOrUndefined (Prefix)
  , "Delimiter" :: NullOrUndefined (Delimiter)
  , "MaxKeys" :: NullOrUndefined (MaxKeys)
  , "CommonPrefixes" :: NullOrUndefined (CommonPrefixList)
  , "EncodingType" :: NullOrUndefined (EncodingType)
  }
derive instance newtypeListObjectVersionsOutput :: Newtype ListObjectVersionsOutput _
derive instance repGenericListObjectVersionsOutput :: Generic ListObjectVersionsOutput _
instance showListObjectVersionsOutput :: Show ListObjectVersionsOutput where show = genericShow
instance decodeListObjectVersionsOutput :: Decode ListObjectVersionsOutput where decode = genericDecode options
instance encodeListObjectVersionsOutput :: Encode ListObjectVersionsOutput where encode = genericEncode options

-- | Constructs ListObjectVersionsOutput from required parameters
newListObjectVersionsOutput :: ListObjectVersionsOutput
newListObjectVersionsOutput  = ListObjectVersionsOutput { "CommonPrefixes": (NullOrUndefined Nothing), "DeleteMarkers": (NullOrUndefined Nothing), "Delimiter": (NullOrUndefined Nothing), "EncodingType": (NullOrUndefined Nothing), "IsTruncated": (NullOrUndefined Nothing), "KeyMarker": (NullOrUndefined Nothing), "MaxKeys": (NullOrUndefined Nothing), "Name": (NullOrUndefined Nothing), "NextKeyMarker": (NullOrUndefined Nothing), "NextVersionIdMarker": (NullOrUndefined Nothing), "Prefix": (NullOrUndefined Nothing), "VersionIdMarker": (NullOrUndefined Nothing), "Versions": (NullOrUndefined Nothing) }

-- | Constructs ListObjectVersionsOutput's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newListObjectVersionsOutput' :: ( { "IsTruncated" :: NullOrUndefined (IsTruncated) , "KeyMarker" :: NullOrUndefined (KeyMarker) , "VersionIdMarker" :: NullOrUndefined (VersionIdMarker) , "NextKeyMarker" :: NullOrUndefined (NextKeyMarker) , "NextVersionIdMarker" :: NullOrUndefined (NextVersionIdMarker) , "Versions" :: NullOrUndefined (ObjectVersionList) , "DeleteMarkers" :: NullOrUndefined (DeleteMarkers) , "Name" :: NullOrUndefined (BucketName) , "Prefix" :: NullOrUndefined (Prefix) , "Delimiter" :: NullOrUndefined (Delimiter) , "MaxKeys" :: NullOrUndefined (MaxKeys) , "CommonPrefixes" :: NullOrUndefined (CommonPrefixList) , "EncodingType" :: NullOrUndefined (EncodingType) } -> {"IsTruncated" :: NullOrUndefined (IsTruncated) , "KeyMarker" :: NullOrUndefined (KeyMarker) , "VersionIdMarker" :: NullOrUndefined (VersionIdMarker) , "NextKeyMarker" :: NullOrUndefined (NextKeyMarker) , "NextVersionIdMarker" :: NullOrUndefined (NextVersionIdMarker) , "Versions" :: NullOrUndefined (ObjectVersionList) , "DeleteMarkers" :: NullOrUndefined (DeleteMarkers) , "Name" :: NullOrUndefined (BucketName) , "Prefix" :: NullOrUndefined (Prefix) , "Delimiter" :: NullOrUndefined (Delimiter) , "MaxKeys" :: NullOrUndefined (MaxKeys) , "CommonPrefixes" :: NullOrUndefined (CommonPrefixList) , "EncodingType" :: NullOrUndefined (EncodingType) } ) -> ListObjectVersionsOutput
newListObjectVersionsOutput'  customize = (ListObjectVersionsOutput <<< customize) { "CommonPrefixes": (NullOrUndefined Nothing), "DeleteMarkers": (NullOrUndefined Nothing), "Delimiter": (NullOrUndefined Nothing), "EncodingType": (NullOrUndefined Nothing), "IsTruncated": (NullOrUndefined Nothing), "KeyMarker": (NullOrUndefined Nothing), "MaxKeys": (NullOrUndefined Nothing), "Name": (NullOrUndefined Nothing), "NextKeyMarker": (NullOrUndefined Nothing), "NextVersionIdMarker": (NullOrUndefined Nothing), "Prefix": (NullOrUndefined Nothing), "VersionIdMarker": (NullOrUndefined Nothing), "Versions": (NullOrUndefined Nothing) }



newtype ListObjectVersionsRequest = ListObjectVersionsRequest 
  { "Bucket" :: (BucketName)
  , "Delimiter" :: NullOrUndefined (Delimiter)
  , "EncodingType" :: NullOrUndefined (EncodingType)
  , "KeyMarker" :: NullOrUndefined (KeyMarker)
  , "MaxKeys" :: NullOrUndefined (MaxKeys)
  , "Prefix" :: NullOrUndefined (Prefix)
  , "VersionIdMarker" :: NullOrUndefined (VersionIdMarker)
  }
derive instance newtypeListObjectVersionsRequest :: Newtype ListObjectVersionsRequest _
derive instance repGenericListObjectVersionsRequest :: Generic ListObjectVersionsRequest _
instance showListObjectVersionsRequest :: Show ListObjectVersionsRequest where show = genericShow
instance decodeListObjectVersionsRequest :: Decode ListObjectVersionsRequest where decode = genericDecode options
instance encodeListObjectVersionsRequest :: Encode ListObjectVersionsRequest where encode = genericEncode options

-- | Constructs ListObjectVersionsRequest from required parameters
newListObjectVersionsRequest :: BucketName -> ListObjectVersionsRequest
newListObjectVersionsRequest _Bucket = ListObjectVersionsRequest { "Bucket": _Bucket, "Delimiter": (NullOrUndefined Nothing), "EncodingType": (NullOrUndefined Nothing), "KeyMarker": (NullOrUndefined Nothing), "MaxKeys": (NullOrUndefined Nothing), "Prefix": (NullOrUndefined Nothing), "VersionIdMarker": (NullOrUndefined Nothing) }

-- | Constructs ListObjectVersionsRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newListObjectVersionsRequest' :: BucketName -> ( { "Bucket" :: (BucketName) , "Delimiter" :: NullOrUndefined (Delimiter) , "EncodingType" :: NullOrUndefined (EncodingType) , "KeyMarker" :: NullOrUndefined (KeyMarker) , "MaxKeys" :: NullOrUndefined (MaxKeys) , "Prefix" :: NullOrUndefined (Prefix) , "VersionIdMarker" :: NullOrUndefined (VersionIdMarker) } -> {"Bucket" :: (BucketName) , "Delimiter" :: NullOrUndefined (Delimiter) , "EncodingType" :: NullOrUndefined (EncodingType) , "KeyMarker" :: NullOrUndefined (KeyMarker) , "MaxKeys" :: NullOrUndefined (MaxKeys) , "Prefix" :: NullOrUndefined (Prefix) , "VersionIdMarker" :: NullOrUndefined (VersionIdMarker) } ) -> ListObjectVersionsRequest
newListObjectVersionsRequest' _Bucket customize = (ListObjectVersionsRequest <<< customize) { "Bucket": _Bucket, "Delimiter": (NullOrUndefined Nothing), "EncodingType": (NullOrUndefined Nothing), "KeyMarker": (NullOrUndefined Nothing), "MaxKeys": (NullOrUndefined Nothing), "Prefix": (NullOrUndefined Nothing), "VersionIdMarker": (NullOrUndefined Nothing) }



newtype ListObjectsOutput = ListObjectsOutput 
  { "IsTruncated" :: NullOrUndefined (IsTruncated)
  , "Marker" :: NullOrUndefined (Marker)
  , "NextMarker" :: NullOrUndefined (NextMarker)
  , "Contents" :: NullOrUndefined (ObjectList)
  , "Name" :: NullOrUndefined (BucketName)
  , "Prefix" :: NullOrUndefined (Prefix)
  , "Delimiter" :: NullOrUndefined (Delimiter)
  , "MaxKeys" :: NullOrUndefined (MaxKeys)
  , "CommonPrefixes" :: NullOrUndefined (CommonPrefixList)
  , "EncodingType" :: NullOrUndefined (EncodingType)
  }
derive instance newtypeListObjectsOutput :: Newtype ListObjectsOutput _
derive instance repGenericListObjectsOutput :: Generic ListObjectsOutput _
instance showListObjectsOutput :: Show ListObjectsOutput where show = genericShow
instance decodeListObjectsOutput :: Decode ListObjectsOutput where decode = genericDecode options
instance encodeListObjectsOutput :: Encode ListObjectsOutput where encode = genericEncode options

-- | Constructs ListObjectsOutput from required parameters
newListObjectsOutput :: ListObjectsOutput
newListObjectsOutput  = ListObjectsOutput { "CommonPrefixes": (NullOrUndefined Nothing), "Contents": (NullOrUndefined Nothing), "Delimiter": (NullOrUndefined Nothing), "EncodingType": (NullOrUndefined Nothing), "IsTruncated": (NullOrUndefined Nothing), "Marker": (NullOrUndefined Nothing), "MaxKeys": (NullOrUndefined Nothing), "Name": (NullOrUndefined Nothing), "NextMarker": (NullOrUndefined Nothing), "Prefix": (NullOrUndefined Nothing) }

-- | Constructs ListObjectsOutput's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newListObjectsOutput' :: ( { "IsTruncated" :: NullOrUndefined (IsTruncated) , "Marker" :: NullOrUndefined (Marker) , "NextMarker" :: NullOrUndefined (NextMarker) , "Contents" :: NullOrUndefined (ObjectList) , "Name" :: NullOrUndefined (BucketName) , "Prefix" :: NullOrUndefined (Prefix) , "Delimiter" :: NullOrUndefined (Delimiter) , "MaxKeys" :: NullOrUndefined (MaxKeys) , "CommonPrefixes" :: NullOrUndefined (CommonPrefixList) , "EncodingType" :: NullOrUndefined (EncodingType) } -> {"IsTruncated" :: NullOrUndefined (IsTruncated) , "Marker" :: NullOrUndefined (Marker) , "NextMarker" :: NullOrUndefined (NextMarker) , "Contents" :: NullOrUndefined (ObjectList) , "Name" :: NullOrUndefined (BucketName) , "Prefix" :: NullOrUndefined (Prefix) , "Delimiter" :: NullOrUndefined (Delimiter) , "MaxKeys" :: NullOrUndefined (MaxKeys) , "CommonPrefixes" :: NullOrUndefined (CommonPrefixList) , "EncodingType" :: NullOrUndefined (EncodingType) } ) -> ListObjectsOutput
newListObjectsOutput'  customize = (ListObjectsOutput <<< customize) { "CommonPrefixes": (NullOrUndefined Nothing), "Contents": (NullOrUndefined Nothing), "Delimiter": (NullOrUndefined Nothing), "EncodingType": (NullOrUndefined Nothing), "IsTruncated": (NullOrUndefined Nothing), "Marker": (NullOrUndefined Nothing), "MaxKeys": (NullOrUndefined Nothing), "Name": (NullOrUndefined Nothing), "NextMarker": (NullOrUndefined Nothing), "Prefix": (NullOrUndefined Nothing) }



newtype ListObjectsRequest = ListObjectsRequest 
  { "Bucket" :: (BucketName)
  , "Delimiter" :: NullOrUndefined (Delimiter)
  , "EncodingType" :: NullOrUndefined (EncodingType)
  , "Marker" :: NullOrUndefined (Marker)
  , "MaxKeys" :: NullOrUndefined (MaxKeys)
  , "Prefix" :: NullOrUndefined (Prefix)
  , "RequestPayer" :: NullOrUndefined (RequestPayer)
  }
derive instance newtypeListObjectsRequest :: Newtype ListObjectsRequest _
derive instance repGenericListObjectsRequest :: Generic ListObjectsRequest _
instance showListObjectsRequest :: Show ListObjectsRequest where show = genericShow
instance decodeListObjectsRequest :: Decode ListObjectsRequest where decode = genericDecode options
instance encodeListObjectsRequest :: Encode ListObjectsRequest where encode = genericEncode options

-- | Constructs ListObjectsRequest from required parameters
newListObjectsRequest :: BucketName -> ListObjectsRequest
newListObjectsRequest _Bucket = ListObjectsRequest { "Bucket": _Bucket, "Delimiter": (NullOrUndefined Nothing), "EncodingType": (NullOrUndefined Nothing), "Marker": (NullOrUndefined Nothing), "MaxKeys": (NullOrUndefined Nothing), "Prefix": (NullOrUndefined Nothing), "RequestPayer": (NullOrUndefined Nothing) }

-- | Constructs ListObjectsRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newListObjectsRequest' :: BucketName -> ( { "Bucket" :: (BucketName) , "Delimiter" :: NullOrUndefined (Delimiter) , "EncodingType" :: NullOrUndefined (EncodingType) , "Marker" :: NullOrUndefined (Marker) , "MaxKeys" :: NullOrUndefined (MaxKeys) , "Prefix" :: NullOrUndefined (Prefix) , "RequestPayer" :: NullOrUndefined (RequestPayer) } -> {"Bucket" :: (BucketName) , "Delimiter" :: NullOrUndefined (Delimiter) , "EncodingType" :: NullOrUndefined (EncodingType) , "Marker" :: NullOrUndefined (Marker) , "MaxKeys" :: NullOrUndefined (MaxKeys) , "Prefix" :: NullOrUndefined (Prefix) , "RequestPayer" :: NullOrUndefined (RequestPayer) } ) -> ListObjectsRequest
newListObjectsRequest' _Bucket customize = (ListObjectsRequest <<< customize) { "Bucket": _Bucket, "Delimiter": (NullOrUndefined Nothing), "EncodingType": (NullOrUndefined Nothing), "Marker": (NullOrUndefined Nothing), "MaxKeys": (NullOrUndefined Nothing), "Prefix": (NullOrUndefined Nothing), "RequestPayer": (NullOrUndefined Nothing) }



newtype ListObjectsV2Output = ListObjectsV2Output 
  { "IsTruncated" :: NullOrUndefined (IsTruncated)
  , "Contents" :: NullOrUndefined (ObjectList)
  , "Name" :: NullOrUndefined (BucketName)
  , "Prefix" :: NullOrUndefined (Prefix)
  , "Delimiter" :: NullOrUndefined (Delimiter)
  , "MaxKeys" :: NullOrUndefined (MaxKeys)
  , "CommonPrefixes" :: NullOrUndefined (CommonPrefixList)
  , "EncodingType" :: NullOrUndefined (EncodingType)
  , "KeyCount" :: NullOrUndefined (KeyCount)
  , "ContinuationToken" :: NullOrUndefined (Token)
  , "NextContinuationToken" :: NullOrUndefined (NextToken)
  , "StartAfter" :: NullOrUndefined (StartAfter)
  }
derive instance newtypeListObjectsV2Output :: Newtype ListObjectsV2Output _
derive instance repGenericListObjectsV2Output :: Generic ListObjectsV2Output _
instance showListObjectsV2Output :: Show ListObjectsV2Output where show = genericShow
instance decodeListObjectsV2Output :: Decode ListObjectsV2Output where decode = genericDecode options
instance encodeListObjectsV2Output :: Encode ListObjectsV2Output where encode = genericEncode options

-- | Constructs ListObjectsV2Output from required parameters
newListObjectsV2Output :: ListObjectsV2Output
newListObjectsV2Output  = ListObjectsV2Output { "CommonPrefixes": (NullOrUndefined Nothing), "Contents": (NullOrUndefined Nothing), "ContinuationToken": (NullOrUndefined Nothing), "Delimiter": (NullOrUndefined Nothing), "EncodingType": (NullOrUndefined Nothing), "IsTruncated": (NullOrUndefined Nothing), "KeyCount": (NullOrUndefined Nothing), "MaxKeys": (NullOrUndefined Nothing), "Name": (NullOrUndefined Nothing), "NextContinuationToken": (NullOrUndefined Nothing), "Prefix": (NullOrUndefined Nothing), "StartAfter": (NullOrUndefined Nothing) }

-- | Constructs ListObjectsV2Output's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newListObjectsV2Output' :: ( { "IsTruncated" :: NullOrUndefined (IsTruncated) , "Contents" :: NullOrUndefined (ObjectList) , "Name" :: NullOrUndefined (BucketName) , "Prefix" :: NullOrUndefined (Prefix) , "Delimiter" :: NullOrUndefined (Delimiter) , "MaxKeys" :: NullOrUndefined (MaxKeys) , "CommonPrefixes" :: NullOrUndefined (CommonPrefixList) , "EncodingType" :: NullOrUndefined (EncodingType) , "KeyCount" :: NullOrUndefined (KeyCount) , "ContinuationToken" :: NullOrUndefined (Token) , "NextContinuationToken" :: NullOrUndefined (NextToken) , "StartAfter" :: NullOrUndefined (StartAfter) } -> {"IsTruncated" :: NullOrUndefined (IsTruncated) , "Contents" :: NullOrUndefined (ObjectList) , "Name" :: NullOrUndefined (BucketName) , "Prefix" :: NullOrUndefined (Prefix) , "Delimiter" :: NullOrUndefined (Delimiter) , "MaxKeys" :: NullOrUndefined (MaxKeys) , "CommonPrefixes" :: NullOrUndefined (CommonPrefixList) , "EncodingType" :: NullOrUndefined (EncodingType) , "KeyCount" :: NullOrUndefined (KeyCount) , "ContinuationToken" :: NullOrUndefined (Token) , "NextContinuationToken" :: NullOrUndefined (NextToken) , "StartAfter" :: NullOrUndefined (StartAfter) } ) -> ListObjectsV2Output
newListObjectsV2Output'  customize = (ListObjectsV2Output <<< customize) { "CommonPrefixes": (NullOrUndefined Nothing), "Contents": (NullOrUndefined Nothing), "ContinuationToken": (NullOrUndefined Nothing), "Delimiter": (NullOrUndefined Nothing), "EncodingType": (NullOrUndefined Nothing), "IsTruncated": (NullOrUndefined Nothing), "KeyCount": (NullOrUndefined Nothing), "MaxKeys": (NullOrUndefined Nothing), "Name": (NullOrUndefined Nothing), "NextContinuationToken": (NullOrUndefined Nothing), "Prefix": (NullOrUndefined Nothing), "StartAfter": (NullOrUndefined Nothing) }



newtype ListObjectsV2Request = ListObjectsV2Request 
  { "Bucket" :: (BucketName)
  , "Delimiter" :: NullOrUndefined (Delimiter)
  , "EncodingType" :: NullOrUndefined (EncodingType)
  , "MaxKeys" :: NullOrUndefined (MaxKeys)
  , "Prefix" :: NullOrUndefined (Prefix)
  , "ContinuationToken" :: NullOrUndefined (Token)
  , "FetchOwner" :: NullOrUndefined (FetchOwner)
  , "StartAfter" :: NullOrUndefined (StartAfter)
  , "RequestPayer" :: NullOrUndefined (RequestPayer)
  }
derive instance newtypeListObjectsV2Request :: Newtype ListObjectsV2Request _
derive instance repGenericListObjectsV2Request :: Generic ListObjectsV2Request _
instance showListObjectsV2Request :: Show ListObjectsV2Request where show = genericShow
instance decodeListObjectsV2Request :: Decode ListObjectsV2Request where decode = genericDecode options
instance encodeListObjectsV2Request :: Encode ListObjectsV2Request where encode = genericEncode options

-- | Constructs ListObjectsV2Request from required parameters
newListObjectsV2Request :: BucketName -> ListObjectsV2Request
newListObjectsV2Request _Bucket = ListObjectsV2Request { "Bucket": _Bucket, "ContinuationToken": (NullOrUndefined Nothing), "Delimiter": (NullOrUndefined Nothing), "EncodingType": (NullOrUndefined Nothing), "FetchOwner": (NullOrUndefined Nothing), "MaxKeys": (NullOrUndefined Nothing), "Prefix": (NullOrUndefined Nothing), "RequestPayer": (NullOrUndefined Nothing), "StartAfter": (NullOrUndefined Nothing) }

-- | Constructs ListObjectsV2Request's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newListObjectsV2Request' :: BucketName -> ( { "Bucket" :: (BucketName) , "Delimiter" :: NullOrUndefined (Delimiter) , "EncodingType" :: NullOrUndefined (EncodingType) , "MaxKeys" :: NullOrUndefined (MaxKeys) , "Prefix" :: NullOrUndefined (Prefix) , "ContinuationToken" :: NullOrUndefined (Token) , "FetchOwner" :: NullOrUndefined (FetchOwner) , "StartAfter" :: NullOrUndefined (StartAfter) , "RequestPayer" :: NullOrUndefined (RequestPayer) } -> {"Bucket" :: (BucketName) , "Delimiter" :: NullOrUndefined (Delimiter) , "EncodingType" :: NullOrUndefined (EncodingType) , "MaxKeys" :: NullOrUndefined (MaxKeys) , "Prefix" :: NullOrUndefined (Prefix) , "ContinuationToken" :: NullOrUndefined (Token) , "FetchOwner" :: NullOrUndefined (FetchOwner) , "StartAfter" :: NullOrUndefined (StartAfter) , "RequestPayer" :: NullOrUndefined (RequestPayer) } ) -> ListObjectsV2Request
newListObjectsV2Request' _Bucket customize = (ListObjectsV2Request <<< customize) { "Bucket": _Bucket, "ContinuationToken": (NullOrUndefined Nothing), "Delimiter": (NullOrUndefined Nothing), "EncodingType": (NullOrUndefined Nothing), "FetchOwner": (NullOrUndefined Nothing), "MaxKeys": (NullOrUndefined Nothing), "Prefix": (NullOrUndefined Nothing), "RequestPayer": (NullOrUndefined Nothing), "StartAfter": (NullOrUndefined Nothing) }



newtype ListPartsOutput = ListPartsOutput 
  { "AbortDate" :: NullOrUndefined (AbortDate)
  , "AbortRuleId" :: NullOrUndefined (AbortRuleId)
  , "Bucket" :: NullOrUndefined (BucketName)
  , "Key" :: NullOrUndefined (ObjectKey)
  , "UploadId" :: NullOrUndefined (MultipartUploadId)
  , "PartNumberMarker" :: NullOrUndefined (PartNumberMarker)
  , "NextPartNumberMarker" :: NullOrUndefined (NextPartNumberMarker)
  , "MaxParts" :: NullOrUndefined (MaxParts)
  , "IsTruncated" :: NullOrUndefined (IsTruncated)
  , "Parts" :: NullOrUndefined (Parts)
  , "Initiator" :: NullOrUndefined (Initiator)
  , "Owner" :: NullOrUndefined (Owner)
  , "StorageClass" :: NullOrUndefined (StorageClass)
  , "RequestCharged" :: NullOrUndefined (RequestCharged)
  }
derive instance newtypeListPartsOutput :: Newtype ListPartsOutput _
derive instance repGenericListPartsOutput :: Generic ListPartsOutput _
instance showListPartsOutput :: Show ListPartsOutput where show = genericShow
instance decodeListPartsOutput :: Decode ListPartsOutput where decode = genericDecode options
instance encodeListPartsOutput :: Encode ListPartsOutput where encode = genericEncode options

-- | Constructs ListPartsOutput from required parameters
newListPartsOutput :: ListPartsOutput
newListPartsOutput  = ListPartsOutput { "AbortDate": (NullOrUndefined Nothing), "AbortRuleId": (NullOrUndefined Nothing), "Bucket": (NullOrUndefined Nothing), "Initiator": (NullOrUndefined Nothing), "IsTruncated": (NullOrUndefined Nothing), "Key": (NullOrUndefined Nothing), "MaxParts": (NullOrUndefined Nothing), "NextPartNumberMarker": (NullOrUndefined Nothing), "Owner": (NullOrUndefined Nothing), "PartNumberMarker": (NullOrUndefined Nothing), "Parts": (NullOrUndefined Nothing), "RequestCharged": (NullOrUndefined Nothing), "StorageClass": (NullOrUndefined Nothing), "UploadId": (NullOrUndefined Nothing) }

-- | Constructs ListPartsOutput's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newListPartsOutput' :: ( { "AbortDate" :: NullOrUndefined (AbortDate) , "AbortRuleId" :: NullOrUndefined (AbortRuleId) , "Bucket" :: NullOrUndefined (BucketName) , "Key" :: NullOrUndefined (ObjectKey) , "UploadId" :: NullOrUndefined (MultipartUploadId) , "PartNumberMarker" :: NullOrUndefined (PartNumberMarker) , "NextPartNumberMarker" :: NullOrUndefined (NextPartNumberMarker) , "MaxParts" :: NullOrUndefined (MaxParts) , "IsTruncated" :: NullOrUndefined (IsTruncated) , "Parts" :: NullOrUndefined (Parts) , "Initiator" :: NullOrUndefined (Initiator) , "Owner" :: NullOrUndefined (Owner) , "StorageClass" :: NullOrUndefined (StorageClass) , "RequestCharged" :: NullOrUndefined (RequestCharged) } -> {"AbortDate" :: NullOrUndefined (AbortDate) , "AbortRuleId" :: NullOrUndefined (AbortRuleId) , "Bucket" :: NullOrUndefined (BucketName) , "Key" :: NullOrUndefined (ObjectKey) , "UploadId" :: NullOrUndefined (MultipartUploadId) , "PartNumberMarker" :: NullOrUndefined (PartNumberMarker) , "NextPartNumberMarker" :: NullOrUndefined (NextPartNumberMarker) , "MaxParts" :: NullOrUndefined (MaxParts) , "IsTruncated" :: NullOrUndefined (IsTruncated) , "Parts" :: NullOrUndefined (Parts) , "Initiator" :: NullOrUndefined (Initiator) , "Owner" :: NullOrUndefined (Owner) , "StorageClass" :: NullOrUndefined (StorageClass) , "RequestCharged" :: NullOrUndefined (RequestCharged) } ) -> ListPartsOutput
newListPartsOutput'  customize = (ListPartsOutput <<< customize) { "AbortDate": (NullOrUndefined Nothing), "AbortRuleId": (NullOrUndefined Nothing), "Bucket": (NullOrUndefined Nothing), "Initiator": (NullOrUndefined Nothing), "IsTruncated": (NullOrUndefined Nothing), "Key": (NullOrUndefined Nothing), "MaxParts": (NullOrUndefined Nothing), "NextPartNumberMarker": (NullOrUndefined Nothing), "Owner": (NullOrUndefined Nothing), "PartNumberMarker": (NullOrUndefined Nothing), "Parts": (NullOrUndefined Nothing), "RequestCharged": (NullOrUndefined Nothing), "StorageClass": (NullOrUndefined Nothing), "UploadId": (NullOrUndefined Nothing) }



newtype ListPartsRequest = ListPartsRequest 
  { "Bucket" :: (BucketName)
  , "Key" :: (ObjectKey)
  , "MaxParts" :: NullOrUndefined (MaxParts)
  , "PartNumberMarker" :: NullOrUndefined (PartNumberMarker)
  , "UploadId" :: (MultipartUploadId)
  , "RequestPayer" :: NullOrUndefined (RequestPayer)
  }
derive instance newtypeListPartsRequest :: Newtype ListPartsRequest _
derive instance repGenericListPartsRequest :: Generic ListPartsRequest _
instance showListPartsRequest :: Show ListPartsRequest where show = genericShow
instance decodeListPartsRequest :: Decode ListPartsRequest where decode = genericDecode options
instance encodeListPartsRequest :: Encode ListPartsRequest where encode = genericEncode options

-- | Constructs ListPartsRequest from required parameters
newListPartsRequest :: BucketName -> ObjectKey -> MultipartUploadId -> ListPartsRequest
newListPartsRequest _Bucket _Key _UploadId = ListPartsRequest { "Bucket": _Bucket, "Key": _Key, "UploadId": _UploadId, "MaxParts": (NullOrUndefined Nothing), "PartNumberMarker": (NullOrUndefined Nothing), "RequestPayer": (NullOrUndefined Nothing) }

-- | Constructs ListPartsRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newListPartsRequest' :: BucketName -> ObjectKey -> MultipartUploadId -> ( { "Bucket" :: (BucketName) , "Key" :: (ObjectKey) , "MaxParts" :: NullOrUndefined (MaxParts) , "PartNumberMarker" :: NullOrUndefined (PartNumberMarker) , "UploadId" :: (MultipartUploadId) , "RequestPayer" :: NullOrUndefined (RequestPayer) } -> {"Bucket" :: (BucketName) , "Key" :: (ObjectKey) , "MaxParts" :: NullOrUndefined (MaxParts) , "PartNumberMarker" :: NullOrUndefined (PartNumberMarker) , "UploadId" :: (MultipartUploadId) , "RequestPayer" :: NullOrUndefined (RequestPayer) } ) -> ListPartsRequest
newListPartsRequest' _Bucket _Key _UploadId customize = (ListPartsRequest <<< customize) { "Bucket": _Bucket, "Key": _Key, "UploadId": _UploadId, "MaxParts": (NullOrUndefined Nothing), "PartNumberMarker": (NullOrUndefined Nothing), "RequestPayer": (NullOrUndefined Nothing) }



newtype Location = Location String
derive instance newtypeLocation :: Newtype Location _
derive instance repGenericLocation :: Generic Location _
instance showLocation :: Show Location where show = genericShow
instance decodeLocation :: Decode Location where decode = genericDecode options
instance encodeLocation :: Encode Location where encode = genericEncode options



newtype LocationPrefix = LocationPrefix String
derive instance newtypeLocationPrefix :: Newtype LocationPrefix _
derive instance repGenericLocationPrefix :: Generic LocationPrefix _
instance showLocationPrefix :: Show LocationPrefix where show = genericShow
instance decodeLocationPrefix :: Decode LocationPrefix where decode = genericDecode options
instance encodeLocationPrefix :: Encode LocationPrefix where encode = genericEncode options



newtype LoggingEnabled = LoggingEnabled 
  { "TargetBucket" :: NullOrUndefined (TargetBucket)
  , "TargetGrants" :: NullOrUndefined (TargetGrants)
  , "TargetPrefix" :: NullOrUndefined (TargetPrefix)
  }
derive instance newtypeLoggingEnabled :: Newtype LoggingEnabled _
derive instance repGenericLoggingEnabled :: Generic LoggingEnabled _
instance showLoggingEnabled :: Show LoggingEnabled where show = genericShow
instance decodeLoggingEnabled :: Decode LoggingEnabled where decode = genericDecode options
instance encodeLoggingEnabled :: Encode LoggingEnabled where encode = genericEncode options

-- | Constructs LoggingEnabled from required parameters
newLoggingEnabled :: LoggingEnabled
newLoggingEnabled  = LoggingEnabled { "TargetBucket": (NullOrUndefined Nothing), "TargetGrants": (NullOrUndefined Nothing), "TargetPrefix": (NullOrUndefined Nothing) }

-- | Constructs LoggingEnabled's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newLoggingEnabled' :: ( { "TargetBucket" :: NullOrUndefined (TargetBucket) , "TargetGrants" :: NullOrUndefined (TargetGrants) , "TargetPrefix" :: NullOrUndefined (TargetPrefix) } -> {"TargetBucket" :: NullOrUndefined (TargetBucket) , "TargetGrants" :: NullOrUndefined (TargetGrants) , "TargetPrefix" :: NullOrUndefined (TargetPrefix) } ) -> LoggingEnabled
newLoggingEnabled'  customize = (LoggingEnabled <<< customize) { "TargetBucket": (NullOrUndefined Nothing), "TargetGrants": (NullOrUndefined Nothing), "TargetPrefix": (NullOrUndefined Nothing) }



newtype MFA = MFA String
derive instance newtypeMFA :: Newtype MFA _
derive instance repGenericMFA :: Generic MFA _
instance showMFA :: Show MFA where show = genericShow
instance decodeMFA :: Decode MFA where decode = genericDecode options
instance encodeMFA :: Encode MFA where encode = genericEncode options



newtype MFADelete = MFADelete String
derive instance newtypeMFADelete :: Newtype MFADelete _
derive instance repGenericMFADelete :: Generic MFADelete _
instance showMFADelete :: Show MFADelete where show = genericShow
instance decodeMFADelete :: Decode MFADelete where decode = genericDecode options
instance encodeMFADelete :: Encode MFADelete where encode = genericEncode options



newtype MFADeleteStatus = MFADeleteStatus String
derive instance newtypeMFADeleteStatus :: Newtype MFADeleteStatus _
derive instance repGenericMFADeleteStatus :: Generic MFADeleteStatus _
instance showMFADeleteStatus :: Show MFADeleteStatus where show = genericShow
instance decodeMFADeleteStatus :: Decode MFADeleteStatus where decode = genericDecode options
instance encodeMFADeleteStatus :: Encode MFADeleteStatus where encode = genericEncode options



newtype Marker = Marker String
derive instance newtypeMarker :: Newtype Marker _
derive instance repGenericMarker :: Generic Marker _
instance showMarker :: Show Marker where show = genericShow
instance decodeMarker :: Decode Marker where decode = genericDecode options
instance encodeMarker :: Encode Marker where encode = genericEncode options



newtype MaxAgeSeconds = MaxAgeSeconds Int
derive instance newtypeMaxAgeSeconds :: Newtype MaxAgeSeconds _
derive instance repGenericMaxAgeSeconds :: Generic MaxAgeSeconds _
instance showMaxAgeSeconds :: Show MaxAgeSeconds where show = genericShow
instance decodeMaxAgeSeconds :: Decode MaxAgeSeconds where decode = genericDecode options
instance encodeMaxAgeSeconds :: Encode MaxAgeSeconds where encode = genericEncode options



newtype MaxKeys = MaxKeys Int
derive instance newtypeMaxKeys :: Newtype MaxKeys _
derive instance repGenericMaxKeys :: Generic MaxKeys _
instance showMaxKeys :: Show MaxKeys where show = genericShow
instance decodeMaxKeys :: Decode MaxKeys where decode = genericDecode options
instance encodeMaxKeys :: Encode MaxKeys where encode = genericEncode options



newtype MaxParts = MaxParts Int
derive instance newtypeMaxParts :: Newtype MaxParts _
derive instance repGenericMaxParts :: Generic MaxParts _
instance showMaxParts :: Show MaxParts where show = genericShow
instance decodeMaxParts :: Decode MaxParts where decode = genericDecode options
instance encodeMaxParts :: Encode MaxParts where encode = genericEncode options



newtype MaxUploads = MaxUploads Int
derive instance newtypeMaxUploads :: Newtype MaxUploads _
derive instance repGenericMaxUploads :: Generic MaxUploads _
instance showMaxUploads :: Show MaxUploads where show = genericShow
instance decodeMaxUploads :: Decode MaxUploads where decode = genericDecode options
instance encodeMaxUploads :: Encode MaxUploads where encode = genericEncode options



newtype Message = Message String
derive instance newtypeMessage :: Newtype Message _
derive instance repGenericMessage :: Generic Message _
instance showMessage :: Show Message where show = genericShow
instance decodeMessage :: Decode Message where decode = genericDecode options
instance encodeMessage :: Encode Message where encode = genericEncode options



newtype Metadata = Metadata (StrMap.StrMap MetadataValue)
derive instance newtypeMetadata :: Newtype Metadata _
derive instance repGenericMetadata :: Generic Metadata _
instance showMetadata :: Show Metadata where show = genericShow
instance decodeMetadata :: Decode Metadata where decode = genericDecode options
instance encodeMetadata :: Encode Metadata where encode = genericEncode options



newtype MetadataDirective = MetadataDirective String
derive instance newtypeMetadataDirective :: Newtype MetadataDirective _
derive instance repGenericMetadataDirective :: Generic MetadataDirective _
instance showMetadataDirective :: Show MetadataDirective where show = genericShow
instance decodeMetadataDirective :: Decode MetadataDirective where decode = genericDecode options
instance encodeMetadataDirective :: Encode MetadataDirective where encode = genericEncode options



-- | A metadata key-value pair to store with an object.
newtype MetadataEntry = MetadataEntry 
  { "Name" :: NullOrUndefined (MetadataKey)
  , "Value" :: NullOrUndefined (MetadataValue)
  }
derive instance newtypeMetadataEntry :: Newtype MetadataEntry _
derive instance repGenericMetadataEntry :: Generic MetadataEntry _
instance showMetadataEntry :: Show MetadataEntry where show = genericShow
instance decodeMetadataEntry :: Decode MetadataEntry where decode = genericDecode options
instance encodeMetadataEntry :: Encode MetadataEntry where encode = genericEncode options

-- | Constructs MetadataEntry from required parameters
newMetadataEntry :: MetadataEntry
newMetadataEntry  = MetadataEntry { "Name": (NullOrUndefined Nothing), "Value": (NullOrUndefined Nothing) }

-- | Constructs MetadataEntry's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newMetadataEntry' :: ( { "Name" :: NullOrUndefined (MetadataKey) , "Value" :: NullOrUndefined (MetadataValue) } -> {"Name" :: NullOrUndefined (MetadataKey) , "Value" :: NullOrUndefined (MetadataValue) } ) -> MetadataEntry
newMetadataEntry'  customize = (MetadataEntry <<< customize) { "Name": (NullOrUndefined Nothing), "Value": (NullOrUndefined Nothing) }



newtype MetadataKey = MetadataKey String
derive instance newtypeMetadataKey :: Newtype MetadataKey _
derive instance repGenericMetadataKey :: Generic MetadataKey _
instance showMetadataKey :: Show MetadataKey where show = genericShow
instance decodeMetadataKey :: Decode MetadataKey where decode = genericDecode options
instance encodeMetadataKey :: Encode MetadataKey where encode = genericEncode options



newtype MetadataValue = MetadataValue String
derive instance newtypeMetadataValue :: Newtype MetadataValue _
derive instance repGenericMetadataValue :: Generic MetadataValue _
instance showMetadataValue :: Show MetadataValue where show = genericShow
instance decodeMetadataValue :: Decode MetadataValue where decode = genericDecode options
instance encodeMetadataValue :: Encode MetadataValue where encode = genericEncode options



newtype MetricsAndOperator = MetricsAndOperator 
  { "Prefix" :: NullOrUndefined (Prefix)
  , "Tags" :: NullOrUndefined (TagSet)
  }
derive instance newtypeMetricsAndOperator :: Newtype MetricsAndOperator _
derive instance repGenericMetricsAndOperator :: Generic MetricsAndOperator _
instance showMetricsAndOperator :: Show MetricsAndOperator where show = genericShow
instance decodeMetricsAndOperator :: Decode MetricsAndOperator where decode = genericDecode options
instance encodeMetricsAndOperator :: Encode MetricsAndOperator where encode = genericEncode options

-- | Constructs MetricsAndOperator from required parameters
newMetricsAndOperator :: MetricsAndOperator
newMetricsAndOperator  = MetricsAndOperator { "Prefix": (NullOrUndefined Nothing), "Tags": (NullOrUndefined Nothing) }

-- | Constructs MetricsAndOperator's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newMetricsAndOperator' :: ( { "Prefix" :: NullOrUndefined (Prefix) , "Tags" :: NullOrUndefined (TagSet) } -> {"Prefix" :: NullOrUndefined (Prefix) , "Tags" :: NullOrUndefined (TagSet) } ) -> MetricsAndOperator
newMetricsAndOperator'  customize = (MetricsAndOperator <<< customize) { "Prefix": (NullOrUndefined Nothing), "Tags": (NullOrUndefined Nothing) }



newtype MetricsConfiguration = MetricsConfiguration 
  { "Id" :: (MetricsId)
  , "Filter" :: NullOrUndefined (MetricsFilter)
  }
derive instance newtypeMetricsConfiguration :: Newtype MetricsConfiguration _
derive instance repGenericMetricsConfiguration :: Generic MetricsConfiguration _
instance showMetricsConfiguration :: Show MetricsConfiguration where show = genericShow
instance decodeMetricsConfiguration :: Decode MetricsConfiguration where decode = genericDecode options
instance encodeMetricsConfiguration :: Encode MetricsConfiguration where encode = genericEncode options

-- | Constructs MetricsConfiguration from required parameters
newMetricsConfiguration :: MetricsId -> MetricsConfiguration
newMetricsConfiguration _Id = MetricsConfiguration { "Id": _Id, "Filter": (NullOrUndefined Nothing) }

-- | Constructs MetricsConfiguration's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newMetricsConfiguration' :: MetricsId -> ( { "Id" :: (MetricsId) , "Filter" :: NullOrUndefined (MetricsFilter) } -> {"Id" :: (MetricsId) , "Filter" :: NullOrUndefined (MetricsFilter) } ) -> MetricsConfiguration
newMetricsConfiguration' _Id customize = (MetricsConfiguration <<< customize) { "Id": _Id, "Filter": (NullOrUndefined Nothing) }



newtype MetricsConfigurationList = MetricsConfigurationList (Array MetricsConfiguration)
derive instance newtypeMetricsConfigurationList :: Newtype MetricsConfigurationList _
derive instance repGenericMetricsConfigurationList :: Generic MetricsConfigurationList _
instance showMetricsConfigurationList :: Show MetricsConfigurationList where show = genericShow
instance decodeMetricsConfigurationList :: Decode MetricsConfigurationList where decode = genericDecode options
instance encodeMetricsConfigurationList :: Encode MetricsConfigurationList where encode = genericEncode options



newtype MetricsFilter = MetricsFilter 
  { "Prefix" :: NullOrUndefined (Prefix)
  , "Tag" :: NullOrUndefined (Tag)
  , "And" :: NullOrUndefined (MetricsAndOperator)
  }
derive instance newtypeMetricsFilter :: Newtype MetricsFilter _
derive instance repGenericMetricsFilter :: Generic MetricsFilter _
instance showMetricsFilter :: Show MetricsFilter where show = genericShow
instance decodeMetricsFilter :: Decode MetricsFilter where decode = genericDecode options
instance encodeMetricsFilter :: Encode MetricsFilter where encode = genericEncode options

-- | Constructs MetricsFilter from required parameters
newMetricsFilter :: MetricsFilter
newMetricsFilter  = MetricsFilter { "And": (NullOrUndefined Nothing), "Prefix": (NullOrUndefined Nothing), "Tag": (NullOrUndefined Nothing) }

-- | Constructs MetricsFilter's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newMetricsFilter' :: ( { "Prefix" :: NullOrUndefined (Prefix) , "Tag" :: NullOrUndefined (Tag) , "And" :: NullOrUndefined (MetricsAndOperator) } -> {"Prefix" :: NullOrUndefined (Prefix) , "Tag" :: NullOrUndefined (Tag) , "And" :: NullOrUndefined (MetricsAndOperator) } ) -> MetricsFilter
newMetricsFilter'  customize = (MetricsFilter <<< customize) { "And": (NullOrUndefined Nothing), "Prefix": (NullOrUndefined Nothing), "Tag": (NullOrUndefined Nothing) }



newtype MetricsId = MetricsId String
derive instance newtypeMetricsId :: Newtype MetricsId _
derive instance repGenericMetricsId :: Generic MetricsId _
instance showMetricsId :: Show MetricsId where show = genericShow
instance decodeMetricsId :: Decode MetricsId where decode = genericDecode options
instance encodeMetricsId :: Encode MetricsId where encode = genericEncode options



newtype MissingMeta = MissingMeta Int
derive instance newtypeMissingMeta :: Newtype MissingMeta _
derive instance repGenericMissingMeta :: Generic MissingMeta _
instance showMissingMeta :: Show MissingMeta where show = genericShow
instance decodeMissingMeta :: Decode MissingMeta where decode = genericDecode options
instance encodeMissingMeta :: Encode MissingMeta where encode = genericEncode options



newtype MultipartUpload = MultipartUpload 
  { "UploadId" :: NullOrUndefined (MultipartUploadId)
  , "Key" :: NullOrUndefined (ObjectKey)
  , "Initiated" :: NullOrUndefined (Initiated)
  , "StorageClass" :: NullOrUndefined (StorageClass)
  , "Owner" :: NullOrUndefined (Owner)
  , "Initiator" :: NullOrUndefined (Initiator)
  }
derive instance newtypeMultipartUpload :: Newtype MultipartUpload _
derive instance repGenericMultipartUpload :: Generic MultipartUpload _
instance showMultipartUpload :: Show MultipartUpload where show = genericShow
instance decodeMultipartUpload :: Decode MultipartUpload where decode = genericDecode options
instance encodeMultipartUpload :: Encode MultipartUpload where encode = genericEncode options

-- | Constructs MultipartUpload from required parameters
newMultipartUpload :: MultipartUpload
newMultipartUpload  = MultipartUpload { "Initiated": (NullOrUndefined Nothing), "Initiator": (NullOrUndefined Nothing), "Key": (NullOrUndefined Nothing), "Owner": (NullOrUndefined Nothing), "StorageClass": (NullOrUndefined Nothing), "UploadId": (NullOrUndefined Nothing) }

-- | Constructs MultipartUpload's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newMultipartUpload' :: ( { "UploadId" :: NullOrUndefined (MultipartUploadId) , "Key" :: NullOrUndefined (ObjectKey) , "Initiated" :: NullOrUndefined (Initiated) , "StorageClass" :: NullOrUndefined (StorageClass) , "Owner" :: NullOrUndefined (Owner) , "Initiator" :: NullOrUndefined (Initiator) } -> {"UploadId" :: NullOrUndefined (MultipartUploadId) , "Key" :: NullOrUndefined (ObjectKey) , "Initiated" :: NullOrUndefined (Initiated) , "StorageClass" :: NullOrUndefined (StorageClass) , "Owner" :: NullOrUndefined (Owner) , "Initiator" :: NullOrUndefined (Initiator) } ) -> MultipartUpload
newMultipartUpload'  customize = (MultipartUpload <<< customize) { "Initiated": (NullOrUndefined Nothing), "Initiator": (NullOrUndefined Nothing), "Key": (NullOrUndefined Nothing), "Owner": (NullOrUndefined Nothing), "StorageClass": (NullOrUndefined Nothing), "UploadId": (NullOrUndefined Nothing) }



newtype MultipartUploadId = MultipartUploadId String
derive instance newtypeMultipartUploadId :: Newtype MultipartUploadId _
derive instance repGenericMultipartUploadId :: Generic MultipartUploadId _
instance showMultipartUploadId :: Show MultipartUploadId where show = genericShow
instance decodeMultipartUploadId :: Decode MultipartUploadId where decode = genericDecode options
instance encodeMultipartUploadId :: Encode MultipartUploadId where encode = genericEncode options



newtype MultipartUploadList = MultipartUploadList (Array MultipartUpload)
derive instance newtypeMultipartUploadList :: Newtype MultipartUploadList _
derive instance repGenericMultipartUploadList :: Generic MultipartUploadList _
instance showMultipartUploadList :: Show MultipartUploadList where show = genericShow
instance decodeMultipartUploadList :: Decode MultipartUploadList where decode = genericDecode options
instance encodeMultipartUploadList :: Encode MultipartUploadList where encode = genericEncode options



newtype NextKeyMarker = NextKeyMarker String
derive instance newtypeNextKeyMarker :: Newtype NextKeyMarker _
derive instance repGenericNextKeyMarker :: Generic NextKeyMarker _
instance showNextKeyMarker :: Show NextKeyMarker where show = genericShow
instance decodeNextKeyMarker :: Decode NextKeyMarker where decode = genericDecode options
instance encodeNextKeyMarker :: Encode NextKeyMarker where encode = genericEncode options



newtype NextMarker = NextMarker String
derive instance newtypeNextMarker :: Newtype NextMarker _
derive instance repGenericNextMarker :: Generic NextMarker _
instance showNextMarker :: Show NextMarker where show = genericShow
instance decodeNextMarker :: Decode NextMarker where decode = genericDecode options
instance encodeNextMarker :: Encode NextMarker where encode = genericEncode options



newtype NextPartNumberMarker = NextPartNumberMarker Int
derive instance newtypeNextPartNumberMarker :: Newtype NextPartNumberMarker _
derive instance repGenericNextPartNumberMarker :: Generic NextPartNumberMarker _
instance showNextPartNumberMarker :: Show NextPartNumberMarker where show = genericShow
instance decodeNextPartNumberMarker :: Decode NextPartNumberMarker where decode = genericDecode options
instance encodeNextPartNumberMarker :: Encode NextPartNumberMarker where encode = genericEncode options



newtype NextToken = NextToken String
derive instance newtypeNextToken :: Newtype NextToken _
derive instance repGenericNextToken :: Generic NextToken _
instance showNextToken :: Show NextToken where show = genericShow
instance decodeNextToken :: Decode NextToken where decode = genericDecode options
instance encodeNextToken :: Encode NextToken where encode = genericEncode options



newtype NextUploadIdMarker = NextUploadIdMarker String
derive instance newtypeNextUploadIdMarker :: Newtype NextUploadIdMarker _
derive instance repGenericNextUploadIdMarker :: Generic NextUploadIdMarker _
instance showNextUploadIdMarker :: Show NextUploadIdMarker where show = genericShow
instance decodeNextUploadIdMarker :: Decode NextUploadIdMarker where decode = genericDecode options
instance encodeNextUploadIdMarker :: Encode NextUploadIdMarker where encode = genericEncode options



newtype NextVersionIdMarker = NextVersionIdMarker String
derive instance newtypeNextVersionIdMarker :: Newtype NextVersionIdMarker _
derive instance repGenericNextVersionIdMarker :: Generic NextVersionIdMarker _
instance showNextVersionIdMarker :: Show NextVersionIdMarker where show = genericShow
instance decodeNextVersionIdMarker :: Decode NextVersionIdMarker where decode = genericDecode options
instance encodeNextVersionIdMarker :: Encode NextVersionIdMarker where encode = genericEncode options



-- | The specified bucket does not exist.
newtype NoSuchBucket = NoSuchBucket Types.NoArguments
derive instance newtypeNoSuchBucket :: Newtype NoSuchBucket _
derive instance repGenericNoSuchBucket :: Generic NoSuchBucket _
instance showNoSuchBucket :: Show NoSuchBucket where show = genericShow
instance decodeNoSuchBucket :: Decode NoSuchBucket where decode = genericDecode options
instance encodeNoSuchBucket :: Encode NoSuchBucket where encode = genericEncode options



-- | The specified key does not exist.
newtype NoSuchKey = NoSuchKey Types.NoArguments
derive instance newtypeNoSuchKey :: Newtype NoSuchKey _
derive instance repGenericNoSuchKey :: Generic NoSuchKey _
instance showNoSuchKey :: Show NoSuchKey where show = genericShow
instance decodeNoSuchKey :: Decode NoSuchKey where decode = genericDecode options
instance encodeNoSuchKey :: Encode NoSuchKey where encode = genericEncode options



-- | The specified multipart upload does not exist.
newtype NoSuchUpload = NoSuchUpload Types.NoArguments
derive instance newtypeNoSuchUpload :: Newtype NoSuchUpload _
derive instance repGenericNoSuchUpload :: Generic NoSuchUpload _
instance showNoSuchUpload :: Show NoSuchUpload where show = genericShow
instance decodeNoSuchUpload :: Decode NoSuchUpload where decode = genericDecode options
instance encodeNoSuchUpload :: Encode NoSuchUpload where encode = genericEncode options



-- | Specifies when noncurrent object versions expire. Upon expiration, Amazon S3 permanently deletes the noncurrent object versions. You set this lifecycle configuration action on a bucket that has versioning enabled (or suspended) to request that Amazon S3 delete noncurrent object versions at a specific period in the object's lifetime.
newtype NoncurrentVersionExpiration = NoncurrentVersionExpiration 
  { "NoncurrentDays" :: NullOrUndefined (Days)
  }
derive instance newtypeNoncurrentVersionExpiration :: Newtype NoncurrentVersionExpiration _
derive instance repGenericNoncurrentVersionExpiration :: Generic NoncurrentVersionExpiration _
instance showNoncurrentVersionExpiration :: Show NoncurrentVersionExpiration where show = genericShow
instance decodeNoncurrentVersionExpiration :: Decode NoncurrentVersionExpiration where decode = genericDecode options
instance encodeNoncurrentVersionExpiration :: Encode NoncurrentVersionExpiration where encode = genericEncode options

-- | Constructs NoncurrentVersionExpiration from required parameters
newNoncurrentVersionExpiration :: NoncurrentVersionExpiration
newNoncurrentVersionExpiration  = NoncurrentVersionExpiration { "NoncurrentDays": (NullOrUndefined Nothing) }

-- | Constructs NoncurrentVersionExpiration's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newNoncurrentVersionExpiration' :: ( { "NoncurrentDays" :: NullOrUndefined (Days) } -> {"NoncurrentDays" :: NullOrUndefined (Days) } ) -> NoncurrentVersionExpiration
newNoncurrentVersionExpiration'  customize = (NoncurrentVersionExpiration <<< customize) { "NoncurrentDays": (NullOrUndefined Nothing) }



-- | Container for the transition rule that describes when noncurrent objects transition to the STANDARD_IA or GLACIER storage class. If your bucket is versioning-enabled (or versioning is suspended), you can set this action to request that Amazon S3 transition noncurrent object versions to the STANDARD_IA or GLACIER storage class at a specific period in the object's lifetime.
newtype NoncurrentVersionTransition = NoncurrentVersionTransition 
  { "NoncurrentDays" :: NullOrUndefined (Days)
  , "StorageClass" :: NullOrUndefined (TransitionStorageClass)
  }
derive instance newtypeNoncurrentVersionTransition :: Newtype NoncurrentVersionTransition _
derive instance repGenericNoncurrentVersionTransition :: Generic NoncurrentVersionTransition _
instance showNoncurrentVersionTransition :: Show NoncurrentVersionTransition where show = genericShow
instance decodeNoncurrentVersionTransition :: Decode NoncurrentVersionTransition where decode = genericDecode options
instance encodeNoncurrentVersionTransition :: Encode NoncurrentVersionTransition where encode = genericEncode options

-- | Constructs NoncurrentVersionTransition from required parameters
newNoncurrentVersionTransition :: NoncurrentVersionTransition
newNoncurrentVersionTransition  = NoncurrentVersionTransition { "NoncurrentDays": (NullOrUndefined Nothing), "StorageClass": (NullOrUndefined Nothing) }

-- | Constructs NoncurrentVersionTransition's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newNoncurrentVersionTransition' :: ( { "NoncurrentDays" :: NullOrUndefined (Days) , "StorageClass" :: NullOrUndefined (TransitionStorageClass) } -> {"NoncurrentDays" :: NullOrUndefined (Days) , "StorageClass" :: NullOrUndefined (TransitionStorageClass) } ) -> NoncurrentVersionTransition
newNoncurrentVersionTransition'  customize = (NoncurrentVersionTransition <<< customize) { "NoncurrentDays": (NullOrUndefined Nothing), "StorageClass": (NullOrUndefined Nothing) }



newtype NoncurrentVersionTransitionList = NoncurrentVersionTransitionList (Array NoncurrentVersionTransition)
derive instance newtypeNoncurrentVersionTransitionList :: Newtype NoncurrentVersionTransitionList _
derive instance repGenericNoncurrentVersionTransitionList :: Generic NoncurrentVersionTransitionList _
instance showNoncurrentVersionTransitionList :: Show NoncurrentVersionTransitionList where show = genericShow
instance decodeNoncurrentVersionTransitionList :: Decode NoncurrentVersionTransitionList where decode = genericDecode options
instance encodeNoncurrentVersionTransitionList :: Encode NoncurrentVersionTransitionList where encode = genericEncode options



-- | Container for specifying the notification configuration of the bucket. If this element is empty, notifications are turned off on the bucket.
newtype NotificationConfiguration = NotificationConfiguration 
  { "TopicConfigurations" :: NullOrUndefined (TopicConfigurationList)
  , "QueueConfigurations" :: NullOrUndefined (QueueConfigurationList)
  , "LambdaFunctionConfigurations" :: NullOrUndefined (LambdaFunctionConfigurationList)
  }
derive instance newtypeNotificationConfiguration :: Newtype NotificationConfiguration _
derive instance repGenericNotificationConfiguration :: Generic NotificationConfiguration _
instance showNotificationConfiguration :: Show NotificationConfiguration where show = genericShow
instance decodeNotificationConfiguration :: Decode NotificationConfiguration where decode = genericDecode options
instance encodeNotificationConfiguration :: Encode NotificationConfiguration where encode = genericEncode options

-- | Constructs NotificationConfiguration from required parameters
newNotificationConfiguration :: NotificationConfiguration
newNotificationConfiguration  = NotificationConfiguration { "LambdaFunctionConfigurations": (NullOrUndefined Nothing), "QueueConfigurations": (NullOrUndefined Nothing), "TopicConfigurations": (NullOrUndefined Nothing) }

-- | Constructs NotificationConfiguration's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newNotificationConfiguration' :: ( { "TopicConfigurations" :: NullOrUndefined (TopicConfigurationList) , "QueueConfigurations" :: NullOrUndefined (QueueConfigurationList) , "LambdaFunctionConfigurations" :: NullOrUndefined (LambdaFunctionConfigurationList) } -> {"TopicConfigurations" :: NullOrUndefined (TopicConfigurationList) , "QueueConfigurations" :: NullOrUndefined (QueueConfigurationList) , "LambdaFunctionConfigurations" :: NullOrUndefined (LambdaFunctionConfigurationList) } ) -> NotificationConfiguration
newNotificationConfiguration'  customize = (NotificationConfiguration <<< customize) { "LambdaFunctionConfigurations": (NullOrUndefined Nothing), "QueueConfigurations": (NullOrUndefined Nothing), "TopicConfigurations": (NullOrUndefined Nothing) }



newtype NotificationConfigurationDeprecated = NotificationConfigurationDeprecated 
  { "TopicConfiguration" :: NullOrUndefined (TopicConfigurationDeprecated)
  , "QueueConfiguration" :: NullOrUndefined (QueueConfigurationDeprecated)
  , "CloudFunctionConfiguration" :: NullOrUndefined (CloudFunctionConfiguration)
  }
derive instance newtypeNotificationConfigurationDeprecated :: Newtype NotificationConfigurationDeprecated _
derive instance repGenericNotificationConfigurationDeprecated :: Generic NotificationConfigurationDeprecated _
instance showNotificationConfigurationDeprecated :: Show NotificationConfigurationDeprecated where show = genericShow
instance decodeNotificationConfigurationDeprecated :: Decode NotificationConfigurationDeprecated where decode = genericDecode options
instance encodeNotificationConfigurationDeprecated :: Encode NotificationConfigurationDeprecated where encode = genericEncode options

-- | Constructs NotificationConfigurationDeprecated from required parameters
newNotificationConfigurationDeprecated :: NotificationConfigurationDeprecated
newNotificationConfigurationDeprecated  = NotificationConfigurationDeprecated { "CloudFunctionConfiguration": (NullOrUndefined Nothing), "QueueConfiguration": (NullOrUndefined Nothing), "TopicConfiguration": (NullOrUndefined Nothing) }

-- | Constructs NotificationConfigurationDeprecated's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newNotificationConfigurationDeprecated' :: ( { "TopicConfiguration" :: NullOrUndefined (TopicConfigurationDeprecated) , "QueueConfiguration" :: NullOrUndefined (QueueConfigurationDeprecated) , "CloudFunctionConfiguration" :: NullOrUndefined (CloudFunctionConfiguration) } -> {"TopicConfiguration" :: NullOrUndefined (TopicConfigurationDeprecated) , "QueueConfiguration" :: NullOrUndefined (QueueConfigurationDeprecated) , "CloudFunctionConfiguration" :: NullOrUndefined (CloudFunctionConfiguration) } ) -> NotificationConfigurationDeprecated
newNotificationConfigurationDeprecated'  customize = (NotificationConfigurationDeprecated <<< customize) { "CloudFunctionConfiguration": (NullOrUndefined Nothing), "QueueConfiguration": (NullOrUndefined Nothing), "TopicConfiguration": (NullOrUndefined Nothing) }



-- | Container for object key name filtering rules. For information about key name filtering, go to <a href="http://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html">Configuring Event Notifications</a> in the Amazon Simple Storage Service Developer Guide.
newtype NotificationConfigurationFilter = NotificationConfigurationFilter 
  { "Key" :: NullOrUndefined (S3KeyFilter)
  }
derive instance newtypeNotificationConfigurationFilter :: Newtype NotificationConfigurationFilter _
derive instance repGenericNotificationConfigurationFilter :: Generic NotificationConfigurationFilter _
instance showNotificationConfigurationFilter :: Show NotificationConfigurationFilter where show = genericShow
instance decodeNotificationConfigurationFilter :: Decode NotificationConfigurationFilter where decode = genericDecode options
instance encodeNotificationConfigurationFilter :: Encode NotificationConfigurationFilter where encode = genericEncode options

-- | Constructs NotificationConfigurationFilter from required parameters
newNotificationConfigurationFilter :: NotificationConfigurationFilter
newNotificationConfigurationFilter  = NotificationConfigurationFilter { "Key": (NullOrUndefined Nothing) }

-- | Constructs NotificationConfigurationFilter's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newNotificationConfigurationFilter' :: ( { "Key" :: NullOrUndefined (S3KeyFilter) } -> {"Key" :: NullOrUndefined (S3KeyFilter) } ) -> NotificationConfigurationFilter
newNotificationConfigurationFilter'  customize = (NotificationConfigurationFilter <<< customize) { "Key": (NullOrUndefined Nothing) }



-- | Optional unique identifier for configurations in a notification configuration. If you don't provide one, Amazon S3 will assign an ID.
newtype NotificationId = NotificationId String
derive instance newtypeNotificationId :: Newtype NotificationId _
derive instance repGenericNotificationId :: Generic NotificationId _
instance showNotificationId :: Show NotificationId where show = genericShow
instance decodeNotificationId :: Decode NotificationId where decode = genericDecode options
instance encodeNotificationId :: Encode NotificationId where encode = genericEncode options



newtype Object = Object 
  { "Key" :: NullOrUndefined (ObjectKey)
  , "LastModified" :: NullOrUndefined (LastModified)
  , "ETag" :: NullOrUndefined (ETag)
  , "Size" :: NullOrUndefined (Size)
  , "StorageClass" :: NullOrUndefined (ObjectStorageClass)
  , "Owner" :: NullOrUndefined (Owner)
  }
derive instance newtypeObject :: Newtype Object _
derive instance repGenericObject :: Generic Object _
instance showObject :: Show Object where show = genericShow
instance decodeObject :: Decode Object where decode = genericDecode options
instance encodeObject :: Encode Object where encode = genericEncode options

-- | Constructs Object from required parameters
newObject :: Object
newObject  = Object { "ETag": (NullOrUndefined Nothing), "Key": (NullOrUndefined Nothing), "LastModified": (NullOrUndefined Nothing), "Owner": (NullOrUndefined Nothing), "Size": (NullOrUndefined Nothing), "StorageClass": (NullOrUndefined Nothing) }

-- | Constructs Object's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newObject' :: ( { "Key" :: NullOrUndefined (ObjectKey) , "LastModified" :: NullOrUndefined (LastModified) , "ETag" :: NullOrUndefined (ETag) , "Size" :: NullOrUndefined (Size) , "StorageClass" :: NullOrUndefined (ObjectStorageClass) , "Owner" :: NullOrUndefined (Owner) } -> {"Key" :: NullOrUndefined (ObjectKey) , "LastModified" :: NullOrUndefined (LastModified) , "ETag" :: NullOrUndefined (ETag) , "Size" :: NullOrUndefined (Size) , "StorageClass" :: NullOrUndefined (ObjectStorageClass) , "Owner" :: NullOrUndefined (Owner) } ) -> Object
newObject'  customize = (Object <<< customize) { "ETag": (NullOrUndefined Nothing), "Key": (NullOrUndefined Nothing), "LastModified": (NullOrUndefined Nothing), "Owner": (NullOrUndefined Nothing), "Size": (NullOrUndefined Nothing), "StorageClass": (NullOrUndefined Nothing) }



-- | This operation is not allowed against this storage tier
newtype ObjectAlreadyInActiveTierError = ObjectAlreadyInActiveTierError Types.NoArguments
derive instance newtypeObjectAlreadyInActiveTierError :: Newtype ObjectAlreadyInActiveTierError _
derive instance repGenericObjectAlreadyInActiveTierError :: Generic ObjectAlreadyInActiveTierError _
instance showObjectAlreadyInActiveTierError :: Show ObjectAlreadyInActiveTierError where show = genericShow
instance decodeObjectAlreadyInActiveTierError :: Decode ObjectAlreadyInActiveTierError where decode = genericDecode options
instance encodeObjectAlreadyInActiveTierError :: Encode ObjectAlreadyInActiveTierError where encode = genericEncode options



newtype ObjectCannedACL = ObjectCannedACL String
derive instance newtypeObjectCannedACL :: Newtype ObjectCannedACL _
derive instance repGenericObjectCannedACL :: Generic ObjectCannedACL _
instance showObjectCannedACL :: Show ObjectCannedACL where show = genericShow
instance decodeObjectCannedACL :: Decode ObjectCannedACL where decode = genericDecode options
instance encodeObjectCannedACL :: Encode ObjectCannedACL where encode = genericEncode options



newtype ObjectIdentifier = ObjectIdentifier 
  { "Key" :: (ObjectKey)
  , "VersionId" :: NullOrUndefined (ObjectVersionId)
  }
derive instance newtypeObjectIdentifier :: Newtype ObjectIdentifier _
derive instance repGenericObjectIdentifier :: Generic ObjectIdentifier _
instance showObjectIdentifier :: Show ObjectIdentifier where show = genericShow
instance decodeObjectIdentifier :: Decode ObjectIdentifier where decode = genericDecode options
instance encodeObjectIdentifier :: Encode ObjectIdentifier where encode = genericEncode options

-- | Constructs ObjectIdentifier from required parameters
newObjectIdentifier :: ObjectKey -> ObjectIdentifier
newObjectIdentifier _Key = ObjectIdentifier { "Key": _Key, "VersionId": (NullOrUndefined Nothing) }

-- | Constructs ObjectIdentifier's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newObjectIdentifier' :: ObjectKey -> ( { "Key" :: (ObjectKey) , "VersionId" :: NullOrUndefined (ObjectVersionId) } -> {"Key" :: (ObjectKey) , "VersionId" :: NullOrUndefined (ObjectVersionId) } ) -> ObjectIdentifier
newObjectIdentifier' _Key customize = (ObjectIdentifier <<< customize) { "Key": _Key, "VersionId": (NullOrUndefined Nothing) }



newtype ObjectIdentifierList = ObjectIdentifierList (Array ObjectIdentifier)
derive instance newtypeObjectIdentifierList :: Newtype ObjectIdentifierList _
derive instance repGenericObjectIdentifierList :: Generic ObjectIdentifierList _
instance showObjectIdentifierList :: Show ObjectIdentifierList where show = genericShow
instance decodeObjectIdentifierList :: Decode ObjectIdentifierList where decode = genericDecode options
instance encodeObjectIdentifierList :: Encode ObjectIdentifierList where encode = genericEncode options



newtype ObjectKey = ObjectKey String
derive instance newtypeObjectKey :: Newtype ObjectKey _
derive instance repGenericObjectKey :: Generic ObjectKey _
instance showObjectKey :: Show ObjectKey where show = genericShow
instance decodeObjectKey :: Decode ObjectKey where decode = genericDecode options
instance encodeObjectKey :: Encode ObjectKey where encode = genericEncode options



newtype ObjectList = ObjectList (Array Object)
derive instance newtypeObjectList :: Newtype ObjectList _
derive instance repGenericObjectList :: Generic ObjectList _
instance showObjectList :: Show ObjectList where show = genericShow
instance decodeObjectList :: Decode ObjectList where decode = genericDecode options
instance encodeObjectList :: Encode ObjectList where encode = genericEncode options



-- | The source object of the COPY operation is not in the active tier and is only stored in Amazon Glacier.
newtype ObjectNotInActiveTierError = ObjectNotInActiveTierError Types.NoArguments
derive instance newtypeObjectNotInActiveTierError :: Newtype ObjectNotInActiveTierError _
derive instance repGenericObjectNotInActiveTierError :: Generic ObjectNotInActiveTierError _
instance showObjectNotInActiveTierError :: Show ObjectNotInActiveTierError where show = genericShow
instance decodeObjectNotInActiveTierError :: Decode ObjectNotInActiveTierError where decode = genericDecode options
instance encodeObjectNotInActiveTierError :: Encode ObjectNotInActiveTierError where encode = genericEncode options



newtype ObjectStorageClass = ObjectStorageClass String
derive instance newtypeObjectStorageClass :: Newtype ObjectStorageClass _
derive instance repGenericObjectStorageClass :: Generic ObjectStorageClass _
instance showObjectStorageClass :: Show ObjectStorageClass where show = genericShow
instance decodeObjectStorageClass :: Decode ObjectStorageClass where decode = genericDecode options
instance encodeObjectStorageClass :: Encode ObjectStorageClass where encode = genericEncode options



newtype ObjectVersion = ObjectVersion 
  { "ETag" :: NullOrUndefined (ETag)
  , "Size" :: NullOrUndefined (Size)
  , "StorageClass" :: NullOrUndefined (ObjectVersionStorageClass)
  , "Key" :: NullOrUndefined (ObjectKey)
  , "VersionId" :: NullOrUndefined (ObjectVersionId)
  , "IsLatest" :: NullOrUndefined (IsLatest)
  , "LastModified" :: NullOrUndefined (LastModified)
  , "Owner" :: NullOrUndefined (Owner)
  }
derive instance newtypeObjectVersion :: Newtype ObjectVersion _
derive instance repGenericObjectVersion :: Generic ObjectVersion _
instance showObjectVersion :: Show ObjectVersion where show = genericShow
instance decodeObjectVersion :: Decode ObjectVersion where decode = genericDecode options
instance encodeObjectVersion :: Encode ObjectVersion where encode = genericEncode options

-- | Constructs ObjectVersion from required parameters
newObjectVersion :: ObjectVersion
newObjectVersion  = ObjectVersion { "ETag": (NullOrUndefined Nothing), "IsLatest": (NullOrUndefined Nothing), "Key": (NullOrUndefined Nothing), "LastModified": (NullOrUndefined Nothing), "Owner": (NullOrUndefined Nothing), "Size": (NullOrUndefined Nothing), "StorageClass": (NullOrUndefined Nothing), "VersionId": (NullOrUndefined Nothing) }

-- | Constructs ObjectVersion's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newObjectVersion' :: ( { "ETag" :: NullOrUndefined (ETag) , "Size" :: NullOrUndefined (Size) , "StorageClass" :: NullOrUndefined (ObjectVersionStorageClass) , "Key" :: NullOrUndefined (ObjectKey) , "VersionId" :: NullOrUndefined (ObjectVersionId) , "IsLatest" :: NullOrUndefined (IsLatest) , "LastModified" :: NullOrUndefined (LastModified) , "Owner" :: NullOrUndefined (Owner) } -> {"ETag" :: NullOrUndefined (ETag) , "Size" :: NullOrUndefined (Size) , "StorageClass" :: NullOrUndefined (ObjectVersionStorageClass) , "Key" :: NullOrUndefined (ObjectKey) , "VersionId" :: NullOrUndefined (ObjectVersionId) , "IsLatest" :: NullOrUndefined (IsLatest) , "LastModified" :: NullOrUndefined (LastModified) , "Owner" :: NullOrUndefined (Owner) } ) -> ObjectVersion
newObjectVersion'  customize = (ObjectVersion <<< customize) { "ETag": (NullOrUndefined Nothing), "IsLatest": (NullOrUndefined Nothing), "Key": (NullOrUndefined Nothing), "LastModified": (NullOrUndefined Nothing), "Owner": (NullOrUndefined Nothing), "Size": (NullOrUndefined Nothing), "StorageClass": (NullOrUndefined Nothing), "VersionId": (NullOrUndefined Nothing) }



newtype ObjectVersionId = ObjectVersionId String
derive instance newtypeObjectVersionId :: Newtype ObjectVersionId _
derive instance repGenericObjectVersionId :: Generic ObjectVersionId _
instance showObjectVersionId :: Show ObjectVersionId where show = genericShow
instance decodeObjectVersionId :: Decode ObjectVersionId where decode = genericDecode options
instance encodeObjectVersionId :: Encode ObjectVersionId where encode = genericEncode options



newtype ObjectVersionList = ObjectVersionList (Array ObjectVersion)
derive instance newtypeObjectVersionList :: Newtype ObjectVersionList _
derive instance repGenericObjectVersionList :: Generic ObjectVersionList _
instance showObjectVersionList :: Show ObjectVersionList where show = genericShow
instance decodeObjectVersionList :: Decode ObjectVersionList where decode = genericDecode options
instance encodeObjectVersionList :: Encode ObjectVersionList where encode = genericEncode options



newtype ObjectVersionStorageClass = ObjectVersionStorageClass String
derive instance newtypeObjectVersionStorageClass :: Newtype ObjectVersionStorageClass _
derive instance repGenericObjectVersionStorageClass :: Generic ObjectVersionStorageClass _
instance showObjectVersionStorageClass :: Show ObjectVersionStorageClass where show = genericShow
instance decodeObjectVersionStorageClass :: Decode ObjectVersionStorageClass where decode = genericDecode options
instance encodeObjectVersionStorageClass :: Encode ObjectVersionStorageClass where encode = genericEncode options



-- | Describes the location where the restore job's output is stored.
newtype OutputLocation = OutputLocation 
  { "S3" :: NullOrUndefined (S3Location)
  }
derive instance newtypeOutputLocation :: Newtype OutputLocation _
derive instance repGenericOutputLocation :: Generic OutputLocation _
instance showOutputLocation :: Show OutputLocation where show = genericShow
instance decodeOutputLocation :: Decode OutputLocation where decode = genericDecode options
instance encodeOutputLocation :: Encode OutputLocation where encode = genericEncode options

-- | Constructs OutputLocation from required parameters
newOutputLocation :: OutputLocation
newOutputLocation  = OutputLocation { "S3": (NullOrUndefined Nothing) }

-- | Constructs OutputLocation's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newOutputLocation' :: ( { "S3" :: NullOrUndefined (S3Location) } -> {"S3" :: NullOrUndefined (S3Location) } ) -> OutputLocation
newOutputLocation'  customize = (OutputLocation <<< customize) { "S3": (NullOrUndefined Nothing) }



-- | Describes how results of the Select job are serialized.
newtype OutputSerialization = OutputSerialization 
  { "CSV" :: NullOrUndefined (CSVOutput)
  }
derive instance newtypeOutputSerialization :: Newtype OutputSerialization _
derive instance repGenericOutputSerialization :: Generic OutputSerialization _
instance showOutputSerialization :: Show OutputSerialization where show = genericShow
instance decodeOutputSerialization :: Decode OutputSerialization where decode = genericDecode options
instance encodeOutputSerialization :: Encode OutputSerialization where encode = genericEncode options

-- | Constructs OutputSerialization from required parameters
newOutputSerialization :: OutputSerialization
newOutputSerialization  = OutputSerialization { "CSV": (NullOrUndefined Nothing) }

-- | Constructs OutputSerialization's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newOutputSerialization' :: ( { "CSV" :: NullOrUndefined (CSVOutput) } -> {"CSV" :: NullOrUndefined (CSVOutput) } ) -> OutputSerialization
newOutputSerialization'  customize = (OutputSerialization <<< customize) { "CSV": (NullOrUndefined Nothing) }



newtype Owner = Owner 
  { "DisplayName" :: NullOrUndefined (DisplayName)
  , "ID" :: NullOrUndefined (ID)
  }
derive instance newtypeOwner :: Newtype Owner _
derive instance repGenericOwner :: Generic Owner _
instance showOwner :: Show Owner where show = genericShow
instance decodeOwner :: Decode Owner where decode = genericDecode options
instance encodeOwner :: Encode Owner where encode = genericEncode options

-- | Constructs Owner from required parameters
newOwner :: Owner
newOwner  = Owner { "DisplayName": (NullOrUndefined Nothing), "ID": (NullOrUndefined Nothing) }

-- | Constructs Owner's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newOwner' :: ( { "DisplayName" :: NullOrUndefined (DisplayName) , "ID" :: NullOrUndefined (ID) } -> {"DisplayName" :: NullOrUndefined (DisplayName) , "ID" :: NullOrUndefined (ID) } ) -> Owner
newOwner'  customize = (Owner <<< customize) { "DisplayName": (NullOrUndefined Nothing), "ID": (NullOrUndefined Nothing) }



newtype OwnerOverride = OwnerOverride String
derive instance newtypeOwnerOverride :: Newtype OwnerOverride _
derive instance repGenericOwnerOverride :: Generic OwnerOverride _
instance showOwnerOverride :: Show OwnerOverride where show = genericShow
instance decodeOwnerOverride :: Decode OwnerOverride where decode = genericDecode options
instance encodeOwnerOverride :: Encode OwnerOverride where encode = genericEncode options



newtype Part = Part 
  { "PartNumber" :: NullOrUndefined (PartNumber)
  , "LastModified" :: NullOrUndefined (LastModified)
  , "ETag" :: NullOrUndefined (ETag)
  , "Size" :: NullOrUndefined (Size)
  }
derive instance newtypePart :: Newtype Part _
derive instance repGenericPart :: Generic Part _
instance showPart :: Show Part where show = genericShow
instance decodePart :: Decode Part where decode = genericDecode options
instance encodePart :: Encode Part where encode = genericEncode options

-- | Constructs Part from required parameters
newPart :: Part
newPart  = Part { "ETag": (NullOrUndefined Nothing), "LastModified": (NullOrUndefined Nothing), "PartNumber": (NullOrUndefined Nothing), "Size": (NullOrUndefined Nothing) }

-- | Constructs Part's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newPart' :: ( { "PartNumber" :: NullOrUndefined (PartNumber) , "LastModified" :: NullOrUndefined (LastModified) , "ETag" :: NullOrUndefined (ETag) , "Size" :: NullOrUndefined (Size) } -> {"PartNumber" :: NullOrUndefined (PartNumber) , "LastModified" :: NullOrUndefined (LastModified) , "ETag" :: NullOrUndefined (ETag) , "Size" :: NullOrUndefined (Size) } ) -> Part
newPart'  customize = (Part <<< customize) { "ETag": (NullOrUndefined Nothing), "LastModified": (NullOrUndefined Nothing), "PartNumber": (NullOrUndefined Nothing), "Size": (NullOrUndefined Nothing) }



newtype PartNumber = PartNumber Int
derive instance newtypePartNumber :: Newtype PartNumber _
derive instance repGenericPartNumber :: Generic PartNumber _
instance showPartNumber :: Show PartNumber where show = genericShow
instance decodePartNumber :: Decode PartNumber where decode = genericDecode options
instance encodePartNumber :: Encode PartNumber where encode = genericEncode options



newtype PartNumberMarker = PartNumberMarker Int
derive instance newtypePartNumberMarker :: Newtype PartNumberMarker _
derive instance repGenericPartNumberMarker :: Generic PartNumberMarker _
instance showPartNumberMarker :: Show PartNumberMarker where show = genericShow
instance decodePartNumberMarker :: Decode PartNumberMarker where decode = genericDecode options
instance encodePartNumberMarker :: Encode PartNumberMarker where encode = genericEncode options



newtype Parts = Parts (Array Part)
derive instance newtypeParts :: Newtype Parts _
derive instance repGenericParts :: Generic Parts _
instance showParts :: Show Parts where show = genericShow
instance decodeParts :: Decode Parts where decode = genericDecode options
instance encodeParts :: Encode Parts where encode = genericEncode options



newtype PartsCount = PartsCount Int
derive instance newtypePartsCount :: Newtype PartsCount _
derive instance repGenericPartsCount :: Generic PartsCount _
instance showPartsCount :: Show PartsCount where show = genericShow
instance decodePartsCount :: Decode PartsCount where decode = genericDecode options
instance encodePartsCount :: Encode PartsCount where encode = genericEncode options



newtype Payer = Payer String
derive instance newtypePayer :: Newtype Payer _
derive instance repGenericPayer :: Generic Payer _
instance showPayer :: Show Payer where show = genericShow
instance decodePayer :: Decode Payer where decode = genericDecode options
instance encodePayer :: Encode Payer where encode = genericEncode options



newtype Permission = Permission String
derive instance newtypePermission :: Newtype Permission _
derive instance repGenericPermission :: Generic Permission _
instance showPermission :: Show Permission where show = genericShow
instance decodePermission :: Decode Permission where decode = genericDecode options
instance encodePermission :: Encode Permission where encode = genericEncode options



newtype Policy = Policy String
derive instance newtypePolicy :: Newtype Policy _
derive instance repGenericPolicy :: Generic Policy _
instance showPolicy :: Show Policy where show = genericShow
instance decodePolicy :: Decode Policy where decode = genericDecode options
instance encodePolicy :: Encode Policy where encode = genericEncode options



newtype Prefix = Prefix String
derive instance newtypePrefix :: Newtype Prefix _
derive instance repGenericPrefix :: Generic Prefix _
instance showPrefix :: Show Prefix where show = genericShow
instance decodePrefix :: Decode Prefix where decode = genericDecode options
instance encodePrefix :: Encode Prefix where encode = genericEncode options



newtype Protocol = Protocol String
derive instance newtypeProtocol :: Newtype Protocol _
derive instance repGenericProtocol :: Generic Protocol _
instance showProtocol :: Show Protocol where show = genericShow
instance decodeProtocol :: Decode Protocol where decode = genericDecode options
instance encodeProtocol :: Encode Protocol where encode = genericEncode options



newtype PutBucketAccelerateConfigurationRequest = PutBucketAccelerateConfigurationRequest 
  { "Bucket" :: (BucketName)
  , "AccelerateConfiguration" :: (AccelerateConfiguration)
  }
derive instance newtypePutBucketAccelerateConfigurationRequest :: Newtype PutBucketAccelerateConfigurationRequest _
derive instance repGenericPutBucketAccelerateConfigurationRequest :: Generic PutBucketAccelerateConfigurationRequest _
instance showPutBucketAccelerateConfigurationRequest :: Show PutBucketAccelerateConfigurationRequest where show = genericShow
instance decodePutBucketAccelerateConfigurationRequest :: Decode PutBucketAccelerateConfigurationRequest where decode = genericDecode options
instance encodePutBucketAccelerateConfigurationRequest :: Encode PutBucketAccelerateConfigurationRequest where encode = genericEncode options

-- | Constructs PutBucketAccelerateConfigurationRequest from required parameters
newPutBucketAccelerateConfigurationRequest :: AccelerateConfiguration -> BucketName -> PutBucketAccelerateConfigurationRequest
newPutBucketAccelerateConfigurationRequest _AccelerateConfiguration _Bucket = PutBucketAccelerateConfigurationRequest { "AccelerateConfiguration": _AccelerateConfiguration, "Bucket": _Bucket }

-- | Constructs PutBucketAccelerateConfigurationRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newPutBucketAccelerateConfigurationRequest' :: AccelerateConfiguration -> BucketName -> ( { "Bucket" :: (BucketName) , "AccelerateConfiguration" :: (AccelerateConfiguration) } -> {"Bucket" :: (BucketName) , "AccelerateConfiguration" :: (AccelerateConfiguration) } ) -> PutBucketAccelerateConfigurationRequest
newPutBucketAccelerateConfigurationRequest' _AccelerateConfiguration _Bucket customize = (PutBucketAccelerateConfigurationRequest <<< customize) { "AccelerateConfiguration": _AccelerateConfiguration, "Bucket": _Bucket }



newtype PutBucketAclRequest = PutBucketAclRequest 
  { "ACL" :: NullOrUndefined (BucketCannedACL)
  , "AccessControlPolicy" :: NullOrUndefined (AccessControlPolicy)
  , "Bucket" :: (BucketName)
  , "ContentMD5" :: NullOrUndefined (ContentMD5)
  , "GrantFullControl" :: NullOrUndefined (GrantFullControl)
  , "GrantRead" :: NullOrUndefined (GrantRead)
  , "GrantReadACP" :: NullOrUndefined (GrantReadACP)
  , "GrantWrite" :: NullOrUndefined (GrantWrite)
  , "GrantWriteACP" :: NullOrUndefined (GrantWriteACP)
  }
derive instance newtypePutBucketAclRequest :: Newtype PutBucketAclRequest _
derive instance repGenericPutBucketAclRequest :: Generic PutBucketAclRequest _
instance showPutBucketAclRequest :: Show PutBucketAclRequest where show = genericShow
instance decodePutBucketAclRequest :: Decode PutBucketAclRequest where decode = genericDecode options
instance encodePutBucketAclRequest :: Encode PutBucketAclRequest where encode = genericEncode options

-- | Constructs PutBucketAclRequest from required parameters
newPutBucketAclRequest :: BucketName -> PutBucketAclRequest
newPutBucketAclRequest _Bucket = PutBucketAclRequest { "Bucket": _Bucket, "ACL": (NullOrUndefined Nothing), "AccessControlPolicy": (NullOrUndefined Nothing), "ContentMD5": (NullOrUndefined Nothing), "GrantFullControl": (NullOrUndefined Nothing), "GrantRead": (NullOrUndefined Nothing), "GrantReadACP": (NullOrUndefined Nothing), "GrantWrite": (NullOrUndefined Nothing), "GrantWriteACP": (NullOrUndefined Nothing) }

-- | Constructs PutBucketAclRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newPutBucketAclRequest' :: BucketName -> ( { "ACL" :: NullOrUndefined (BucketCannedACL) , "AccessControlPolicy" :: NullOrUndefined (AccessControlPolicy) , "Bucket" :: (BucketName) , "ContentMD5" :: NullOrUndefined (ContentMD5) , "GrantFullControl" :: NullOrUndefined (GrantFullControl) , "GrantRead" :: NullOrUndefined (GrantRead) , "GrantReadACP" :: NullOrUndefined (GrantReadACP) , "GrantWrite" :: NullOrUndefined (GrantWrite) , "GrantWriteACP" :: NullOrUndefined (GrantWriteACP) } -> {"ACL" :: NullOrUndefined (BucketCannedACL) , "AccessControlPolicy" :: NullOrUndefined (AccessControlPolicy) , "Bucket" :: (BucketName) , "ContentMD5" :: NullOrUndefined (ContentMD5) , "GrantFullControl" :: NullOrUndefined (GrantFullControl) , "GrantRead" :: NullOrUndefined (GrantRead) , "GrantReadACP" :: NullOrUndefined (GrantReadACP) , "GrantWrite" :: NullOrUndefined (GrantWrite) , "GrantWriteACP" :: NullOrUndefined (GrantWriteACP) } ) -> PutBucketAclRequest
newPutBucketAclRequest' _Bucket customize = (PutBucketAclRequest <<< customize) { "Bucket": _Bucket, "ACL": (NullOrUndefined Nothing), "AccessControlPolicy": (NullOrUndefined Nothing), "ContentMD5": (NullOrUndefined Nothing), "GrantFullControl": (NullOrUndefined Nothing), "GrantRead": (NullOrUndefined Nothing), "GrantReadACP": (NullOrUndefined Nothing), "GrantWrite": (NullOrUndefined Nothing), "GrantWriteACP": (NullOrUndefined Nothing) }



newtype PutBucketAnalyticsConfigurationRequest = PutBucketAnalyticsConfigurationRequest 
  { "Bucket" :: (BucketName)
  , "Id" :: (AnalyticsId)
  , "AnalyticsConfiguration" :: (AnalyticsConfiguration)
  }
derive instance newtypePutBucketAnalyticsConfigurationRequest :: Newtype PutBucketAnalyticsConfigurationRequest _
derive instance repGenericPutBucketAnalyticsConfigurationRequest :: Generic PutBucketAnalyticsConfigurationRequest _
instance showPutBucketAnalyticsConfigurationRequest :: Show PutBucketAnalyticsConfigurationRequest where show = genericShow
instance decodePutBucketAnalyticsConfigurationRequest :: Decode PutBucketAnalyticsConfigurationRequest where decode = genericDecode options
instance encodePutBucketAnalyticsConfigurationRequest :: Encode PutBucketAnalyticsConfigurationRequest where encode = genericEncode options

-- | Constructs PutBucketAnalyticsConfigurationRequest from required parameters
newPutBucketAnalyticsConfigurationRequest :: AnalyticsConfiguration -> BucketName -> AnalyticsId -> PutBucketAnalyticsConfigurationRequest
newPutBucketAnalyticsConfigurationRequest _AnalyticsConfiguration _Bucket _Id = PutBucketAnalyticsConfigurationRequest { "AnalyticsConfiguration": _AnalyticsConfiguration, "Bucket": _Bucket, "Id": _Id }

-- | Constructs PutBucketAnalyticsConfigurationRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newPutBucketAnalyticsConfigurationRequest' :: AnalyticsConfiguration -> BucketName -> AnalyticsId -> ( { "Bucket" :: (BucketName) , "Id" :: (AnalyticsId) , "AnalyticsConfiguration" :: (AnalyticsConfiguration) } -> {"Bucket" :: (BucketName) , "Id" :: (AnalyticsId) , "AnalyticsConfiguration" :: (AnalyticsConfiguration) } ) -> PutBucketAnalyticsConfigurationRequest
newPutBucketAnalyticsConfigurationRequest' _AnalyticsConfiguration _Bucket _Id customize = (PutBucketAnalyticsConfigurationRequest <<< customize) { "AnalyticsConfiguration": _AnalyticsConfiguration, "Bucket": _Bucket, "Id": _Id }



newtype PutBucketCorsRequest = PutBucketCorsRequest 
  { "Bucket" :: (BucketName)
  , "CORSConfiguration" :: (CORSConfiguration)
  , "ContentMD5" :: NullOrUndefined (ContentMD5)
  }
derive instance newtypePutBucketCorsRequest :: Newtype PutBucketCorsRequest _
derive instance repGenericPutBucketCorsRequest :: Generic PutBucketCorsRequest _
instance showPutBucketCorsRequest :: Show PutBucketCorsRequest where show = genericShow
instance decodePutBucketCorsRequest :: Decode PutBucketCorsRequest where decode = genericDecode options
instance encodePutBucketCorsRequest :: Encode PutBucketCorsRequest where encode = genericEncode options

-- | Constructs PutBucketCorsRequest from required parameters
newPutBucketCorsRequest :: BucketName -> CORSConfiguration -> PutBucketCorsRequest
newPutBucketCorsRequest _Bucket _CORSConfiguration = PutBucketCorsRequest { "Bucket": _Bucket, "CORSConfiguration": _CORSConfiguration, "ContentMD5": (NullOrUndefined Nothing) }

-- | Constructs PutBucketCorsRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newPutBucketCorsRequest' :: BucketName -> CORSConfiguration -> ( { "Bucket" :: (BucketName) , "CORSConfiguration" :: (CORSConfiguration) , "ContentMD5" :: NullOrUndefined (ContentMD5) } -> {"Bucket" :: (BucketName) , "CORSConfiguration" :: (CORSConfiguration) , "ContentMD5" :: NullOrUndefined (ContentMD5) } ) -> PutBucketCorsRequest
newPutBucketCorsRequest' _Bucket _CORSConfiguration customize = (PutBucketCorsRequest <<< customize) { "Bucket": _Bucket, "CORSConfiguration": _CORSConfiguration, "ContentMD5": (NullOrUndefined Nothing) }



newtype PutBucketEncryptionRequest = PutBucketEncryptionRequest 
  { "Bucket" :: (BucketName)
  , "ContentMD5" :: NullOrUndefined (ContentMD5)
  , "ServerSideEncryptionConfiguration" :: (ServerSideEncryptionConfiguration)
  }
derive instance newtypePutBucketEncryptionRequest :: Newtype PutBucketEncryptionRequest _
derive instance repGenericPutBucketEncryptionRequest :: Generic PutBucketEncryptionRequest _
instance showPutBucketEncryptionRequest :: Show PutBucketEncryptionRequest where show = genericShow
instance decodePutBucketEncryptionRequest :: Decode PutBucketEncryptionRequest where decode = genericDecode options
instance encodePutBucketEncryptionRequest :: Encode PutBucketEncryptionRequest where encode = genericEncode options

-- | Constructs PutBucketEncryptionRequest from required parameters
newPutBucketEncryptionRequest :: BucketName -> ServerSideEncryptionConfiguration -> PutBucketEncryptionRequest
newPutBucketEncryptionRequest _Bucket _ServerSideEncryptionConfiguration = PutBucketEncryptionRequest { "Bucket": _Bucket, "ServerSideEncryptionConfiguration": _ServerSideEncryptionConfiguration, "ContentMD5": (NullOrUndefined Nothing) }

-- | Constructs PutBucketEncryptionRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newPutBucketEncryptionRequest' :: BucketName -> ServerSideEncryptionConfiguration -> ( { "Bucket" :: (BucketName) , "ContentMD5" :: NullOrUndefined (ContentMD5) , "ServerSideEncryptionConfiguration" :: (ServerSideEncryptionConfiguration) } -> {"Bucket" :: (BucketName) , "ContentMD5" :: NullOrUndefined (ContentMD5) , "ServerSideEncryptionConfiguration" :: (ServerSideEncryptionConfiguration) } ) -> PutBucketEncryptionRequest
newPutBucketEncryptionRequest' _Bucket _ServerSideEncryptionConfiguration customize = (PutBucketEncryptionRequest <<< customize) { "Bucket": _Bucket, "ServerSideEncryptionConfiguration": _ServerSideEncryptionConfiguration, "ContentMD5": (NullOrUndefined Nothing) }



newtype PutBucketInventoryConfigurationRequest = PutBucketInventoryConfigurationRequest 
  { "Bucket" :: (BucketName)
  , "Id" :: (InventoryId)
  , "InventoryConfiguration" :: (InventoryConfiguration)
  }
derive instance newtypePutBucketInventoryConfigurationRequest :: Newtype PutBucketInventoryConfigurationRequest _
derive instance repGenericPutBucketInventoryConfigurationRequest :: Generic PutBucketInventoryConfigurationRequest _
instance showPutBucketInventoryConfigurationRequest :: Show PutBucketInventoryConfigurationRequest where show = genericShow
instance decodePutBucketInventoryConfigurationRequest :: Decode PutBucketInventoryConfigurationRequest where decode = genericDecode options
instance encodePutBucketInventoryConfigurationRequest :: Encode PutBucketInventoryConfigurationRequest where encode = genericEncode options

-- | Constructs PutBucketInventoryConfigurationRequest from required parameters
newPutBucketInventoryConfigurationRequest :: BucketName -> InventoryId -> InventoryConfiguration -> PutBucketInventoryConfigurationRequest
newPutBucketInventoryConfigurationRequest _Bucket _Id _InventoryConfiguration = PutBucketInventoryConfigurationRequest { "Bucket": _Bucket, "Id": _Id, "InventoryConfiguration": _InventoryConfiguration }

-- | Constructs PutBucketInventoryConfigurationRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newPutBucketInventoryConfigurationRequest' :: BucketName -> InventoryId -> InventoryConfiguration -> ( { "Bucket" :: (BucketName) , "Id" :: (InventoryId) , "InventoryConfiguration" :: (InventoryConfiguration) } -> {"Bucket" :: (BucketName) , "Id" :: (InventoryId) , "InventoryConfiguration" :: (InventoryConfiguration) } ) -> PutBucketInventoryConfigurationRequest
newPutBucketInventoryConfigurationRequest' _Bucket _Id _InventoryConfiguration customize = (PutBucketInventoryConfigurationRequest <<< customize) { "Bucket": _Bucket, "Id": _Id, "InventoryConfiguration": _InventoryConfiguration }



newtype PutBucketLifecycleConfigurationRequest = PutBucketLifecycleConfigurationRequest 
  { "Bucket" :: (BucketName)
  , "LifecycleConfiguration" :: NullOrUndefined (BucketLifecycleConfiguration)
  }
derive instance newtypePutBucketLifecycleConfigurationRequest :: Newtype PutBucketLifecycleConfigurationRequest _
derive instance repGenericPutBucketLifecycleConfigurationRequest :: Generic PutBucketLifecycleConfigurationRequest _
instance showPutBucketLifecycleConfigurationRequest :: Show PutBucketLifecycleConfigurationRequest where show = genericShow
instance decodePutBucketLifecycleConfigurationRequest :: Decode PutBucketLifecycleConfigurationRequest where decode = genericDecode options
instance encodePutBucketLifecycleConfigurationRequest :: Encode PutBucketLifecycleConfigurationRequest where encode = genericEncode options

-- | Constructs PutBucketLifecycleConfigurationRequest from required parameters
newPutBucketLifecycleConfigurationRequest :: BucketName -> PutBucketLifecycleConfigurationRequest
newPutBucketLifecycleConfigurationRequest _Bucket = PutBucketLifecycleConfigurationRequest { "Bucket": _Bucket, "LifecycleConfiguration": (NullOrUndefined Nothing) }

-- | Constructs PutBucketLifecycleConfigurationRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newPutBucketLifecycleConfigurationRequest' :: BucketName -> ( { "Bucket" :: (BucketName) , "LifecycleConfiguration" :: NullOrUndefined (BucketLifecycleConfiguration) } -> {"Bucket" :: (BucketName) , "LifecycleConfiguration" :: NullOrUndefined (BucketLifecycleConfiguration) } ) -> PutBucketLifecycleConfigurationRequest
newPutBucketLifecycleConfigurationRequest' _Bucket customize = (PutBucketLifecycleConfigurationRequest <<< customize) { "Bucket": _Bucket, "LifecycleConfiguration": (NullOrUndefined Nothing) }



newtype PutBucketLifecycleRequest = PutBucketLifecycleRequest 
  { "Bucket" :: (BucketName)
  , "ContentMD5" :: NullOrUndefined (ContentMD5)
  , "LifecycleConfiguration" :: NullOrUndefined (LifecycleConfiguration)
  }
derive instance newtypePutBucketLifecycleRequest :: Newtype PutBucketLifecycleRequest _
derive instance repGenericPutBucketLifecycleRequest :: Generic PutBucketLifecycleRequest _
instance showPutBucketLifecycleRequest :: Show PutBucketLifecycleRequest where show = genericShow
instance decodePutBucketLifecycleRequest :: Decode PutBucketLifecycleRequest where decode = genericDecode options
instance encodePutBucketLifecycleRequest :: Encode PutBucketLifecycleRequest where encode = genericEncode options

-- | Constructs PutBucketLifecycleRequest from required parameters
newPutBucketLifecycleRequest :: BucketName -> PutBucketLifecycleRequest
newPutBucketLifecycleRequest _Bucket = PutBucketLifecycleRequest { "Bucket": _Bucket, "ContentMD5": (NullOrUndefined Nothing), "LifecycleConfiguration": (NullOrUndefined Nothing) }

-- | Constructs PutBucketLifecycleRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newPutBucketLifecycleRequest' :: BucketName -> ( { "Bucket" :: (BucketName) , "ContentMD5" :: NullOrUndefined (ContentMD5) , "LifecycleConfiguration" :: NullOrUndefined (LifecycleConfiguration) } -> {"Bucket" :: (BucketName) , "ContentMD5" :: NullOrUndefined (ContentMD5) , "LifecycleConfiguration" :: NullOrUndefined (LifecycleConfiguration) } ) -> PutBucketLifecycleRequest
newPutBucketLifecycleRequest' _Bucket customize = (PutBucketLifecycleRequest <<< customize) { "Bucket": _Bucket, "ContentMD5": (NullOrUndefined Nothing), "LifecycleConfiguration": (NullOrUndefined Nothing) }



newtype PutBucketLoggingRequest = PutBucketLoggingRequest 
  { "Bucket" :: (BucketName)
  , "BucketLoggingStatus" :: (BucketLoggingStatus)
  , "ContentMD5" :: NullOrUndefined (ContentMD5)
  }
derive instance newtypePutBucketLoggingRequest :: Newtype PutBucketLoggingRequest _
derive instance repGenericPutBucketLoggingRequest :: Generic PutBucketLoggingRequest _
instance showPutBucketLoggingRequest :: Show PutBucketLoggingRequest where show = genericShow
instance decodePutBucketLoggingRequest :: Decode PutBucketLoggingRequest where decode = genericDecode options
instance encodePutBucketLoggingRequest :: Encode PutBucketLoggingRequest where encode = genericEncode options

-- | Constructs PutBucketLoggingRequest from required parameters
newPutBucketLoggingRequest :: BucketName -> BucketLoggingStatus -> PutBucketLoggingRequest
newPutBucketLoggingRequest _Bucket _BucketLoggingStatus = PutBucketLoggingRequest { "Bucket": _Bucket, "BucketLoggingStatus": _BucketLoggingStatus, "ContentMD5": (NullOrUndefined Nothing) }

-- | Constructs PutBucketLoggingRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newPutBucketLoggingRequest' :: BucketName -> BucketLoggingStatus -> ( { "Bucket" :: (BucketName) , "BucketLoggingStatus" :: (BucketLoggingStatus) , "ContentMD5" :: NullOrUndefined (ContentMD5) } -> {"Bucket" :: (BucketName) , "BucketLoggingStatus" :: (BucketLoggingStatus) , "ContentMD5" :: NullOrUndefined (ContentMD5) } ) -> PutBucketLoggingRequest
newPutBucketLoggingRequest' _Bucket _BucketLoggingStatus customize = (PutBucketLoggingRequest <<< customize) { "Bucket": _Bucket, "BucketLoggingStatus": _BucketLoggingStatus, "ContentMD5": (NullOrUndefined Nothing) }



newtype PutBucketMetricsConfigurationRequest = PutBucketMetricsConfigurationRequest 
  { "Bucket" :: (BucketName)
  , "Id" :: (MetricsId)
  , "MetricsConfiguration" :: (MetricsConfiguration)
  }
derive instance newtypePutBucketMetricsConfigurationRequest :: Newtype PutBucketMetricsConfigurationRequest _
derive instance repGenericPutBucketMetricsConfigurationRequest :: Generic PutBucketMetricsConfigurationRequest _
instance showPutBucketMetricsConfigurationRequest :: Show PutBucketMetricsConfigurationRequest where show = genericShow
instance decodePutBucketMetricsConfigurationRequest :: Decode PutBucketMetricsConfigurationRequest where decode = genericDecode options
instance encodePutBucketMetricsConfigurationRequest :: Encode PutBucketMetricsConfigurationRequest where encode = genericEncode options

-- | Constructs PutBucketMetricsConfigurationRequest from required parameters
newPutBucketMetricsConfigurationRequest :: BucketName -> MetricsId -> MetricsConfiguration -> PutBucketMetricsConfigurationRequest
newPutBucketMetricsConfigurationRequest _Bucket _Id _MetricsConfiguration = PutBucketMetricsConfigurationRequest { "Bucket": _Bucket, "Id": _Id, "MetricsConfiguration": _MetricsConfiguration }

-- | Constructs PutBucketMetricsConfigurationRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newPutBucketMetricsConfigurationRequest' :: BucketName -> MetricsId -> MetricsConfiguration -> ( { "Bucket" :: (BucketName) , "Id" :: (MetricsId) , "MetricsConfiguration" :: (MetricsConfiguration) } -> {"Bucket" :: (BucketName) , "Id" :: (MetricsId) , "MetricsConfiguration" :: (MetricsConfiguration) } ) -> PutBucketMetricsConfigurationRequest
newPutBucketMetricsConfigurationRequest' _Bucket _Id _MetricsConfiguration customize = (PutBucketMetricsConfigurationRequest <<< customize) { "Bucket": _Bucket, "Id": _Id, "MetricsConfiguration": _MetricsConfiguration }



newtype PutBucketNotificationConfigurationRequest = PutBucketNotificationConfigurationRequest 
  { "Bucket" :: (BucketName)
  , "NotificationConfiguration" :: (NotificationConfiguration)
  }
derive instance newtypePutBucketNotificationConfigurationRequest :: Newtype PutBucketNotificationConfigurationRequest _
derive instance repGenericPutBucketNotificationConfigurationRequest :: Generic PutBucketNotificationConfigurationRequest _
instance showPutBucketNotificationConfigurationRequest :: Show PutBucketNotificationConfigurationRequest where show = genericShow
instance decodePutBucketNotificationConfigurationRequest :: Decode PutBucketNotificationConfigurationRequest where decode = genericDecode options
instance encodePutBucketNotificationConfigurationRequest :: Encode PutBucketNotificationConfigurationRequest where encode = genericEncode options

-- | Constructs PutBucketNotificationConfigurationRequest from required parameters
newPutBucketNotificationConfigurationRequest :: BucketName -> NotificationConfiguration -> PutBucketNotificationConfigurationRequest
newPutBucketNotificationConfigurationRequest _Bucket _NotificationConfiguration = PutBucketNotificationConfigurationRequest { "Bucket": _Bucket, "NotificationConfiguration": _NotificationConfiguration }

-- | Constructs PutBucketNotificationConfigurationRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newPutBucketNotificationConfigurationRequest' :: BucketName -> NotificationConfiguration -> ( { "Bucket" :: (BucketName) , "NotificationConfiguration" :: (NotificationConfiguration) } -> {"Bucket" :: (BucketName) , "NotificationConfiguration" :: (NotificationConfiguration) } ) -> PutBucketNotificationConfigurationRequest
newPutBucketNotificationConfigurationRequest' _Bucket _NotificationConfiguration customize = (PutBucketNotificationConfigurationRequest <<< customize) { "Bucket": _Bucket, "NotificationConfiguration": _NotificationConfiguration }



newtype PutBucketNotificationRequest = PutBucketNotificationRequest 
  { "Bucket" :: (BucketName)
  , "ContentMD5" :: NullOrUndefined (ContentMD5)
  , "NotificationConfiguration" :: (NotificationConfigurationDeprecated)
  }
derive instance newtypePutBucketNotificationRequest :: Newtype PutBucketNotificationRequest _
derive instance repGenericPutBucketNotificationRequest :: Generic PutBucketNotificationRequest _
instance showPutBucketNotificationRequest :: Show PutBucketNotificationRequest where show = genericShow
instance decodePutBucketNotificationRequest :: Decode PutBucketNotificationRequest where decode = genericDecode options
instance encodePutBucketNotificationRequest :: Encode PutBucketNotificationRequest where encode = genericEncode options

-- | Constructs PutBucketNotificationRequest from required parameters
newPutBucketNotificationRequest :: BucketName -> NotificationConfigurationDeprecated -> PutBucketNotificationRequest
newPutBucketNotificationRequest _Bucket _NotificationConfiguration = PutBucketNotificationRequest { "Bucket": _Bucket, "NotificationConfiguration": _NotificationConfiguration, "ContentMD5": (NullOrUndefined Nothing) }

-- | Constructs PutBucketNotificationRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newPutBucketNotificationRequest' :: BucketName -> NotificationConfigurationDeprecated -> ( { "Bucket" :: (BucketName) , "ContentMD5" :: NullOrUndefined (ContentMD5) , "NotificationConfiguration" :: (NotificationConfigurationDeprecated) } -> {"Bucket" :: (BucketName) , "ContentMD5" :: NullOrUndefined (ContentMD5) , "NotificationConfiguration" :: (NotificationConfigurationDeprecated) } ) -> PutBucketNotificationRequest
newPutBucketNotificationRequest' _Bucket _NotificationConfiguration customize = (PutBucketNotificationRequest <<< customize) { "Bucket": _Bucket, "NotificationConfiguration": _NotificationConfiguration, "ContentMD5": (NullOrUndefined Nothing) }



newtype PutBucketPolicyRequest = PutBucketPolicyRequest 
  { "Bucket" :: (BucketName)
  , "ContentMD5" :: NullOrUndefined (ContentMD5)
  , "ConfirmRemoveSelfBucketAccess" :: NullOrUndefined (ConfirmRemoveSelfBucketAccess)
  , "Policy" :: (Policy)
  }
derive instance newtypePutBucketPolicyRequest :: Newtype PutBucketPolicyRequest _
derive instance repGenericPutBucketPolicyRequest :: Generic PutBucketPolicyRequest _
instance showPutBucketPolicyRequest :: Show PutBucketPolicyRequest where show = genericShow
instance decodePutBucketPolicyRequest :: Decode PutBucketPolicyRequest where decode = genericDecode options
instance encodePutBucketPolicyRequest :: Encode PutBucketPolicyRequest where encode = genericEncode options

-- | Constructs PutBucketPolicyRequest from required parameters
newPutBucketPolicyRequest :: BucketName -> Policy -> PutBucketPolicyRequest
newPutBucketPolicyRequest _Bucket _Policy = PutBucketPolicyRequest { "Bucket": _Bucket, "Policy": _Policy, "ConfirmRemoveSelfBucketAccess": (NullOrUndefined Nothing), "ContentMD5": (NullOrUndefined Nothing) }

-- | Constructs PutBucketPolicyRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newPutBucketPolicyRequest' :: BucketName -> Policy -> ( { "Bucket" :: (BucketName) , "ContentMD5" :: NullOrUndefined (ContentMD5) , "ConfirmRemoveSelfBucketAccess" :: NullOrUndefined (ConfirmRemoveSelfBucketAccess) , "Policy" :: (Policy) } -> {"Bucket" :: (BucketName) , "ContentMD5" :: NullOrUndefined (ContentMD5) , "ConfirmRemoveSelfBucketAccess" :: NullOrUndefined (ConfirmRemoveSelfBucketAccess) , "Policy" :: (Policy) } ) -> PutBucketPolicyRequest
newPutBucketPolicyRequest' _Bucket _Policy customize = (PutBucketPolicyRequest <<< customize) { "Bucket": _Bucket, "Policy": _Policy, "ConfirmRemoveSelfBucketAccess": (NullOrUndefined Nothing), "ContentMD5": (NullOrUndefined Nothing) }



newtype PutBucketReplicationRequest = PutBucketReplicationRequest 
  { "Bucket" :: (BucketName)
  , "ContentMD5" :: NullOrUndefined (ContentMD5)
  , "ReplicationConfiguration" :: (ReplicationConfiguration)
  }
derive instance newtypePutBucketReplicationRequest :: Newtype PutBucketReplicationRequest _
derive instance repGenericPutBucketReplicationRequest :: Generic PutBucketReplicationRequest _
instance showPutBucketReplicationRequest :: Show PutBucketReplicationRequest where show = genericShow
instance decodePutBucketReplicationRequest :: Decode PutBucketReplicationRequest where decode = genericDecode options
instance encodePutBucketReplicationRequest :: Encode PutBucketReplicationRequest where encode = genericEncode options

-- | Constructs PutBucketReplicationRequest from required parameters
newPutBucketReplicationRequest :: BucketName -> ReplicationConfiguration -> PutBucketReplicationRequest
newPutBucketReplicationRequest _Bucket _ReplicationConfiguration = PutBucketReplicationRequest { "Bucket": _Bucket, "ReplicationConfiguration": _ReplicationConfiguration, "ContentMD5": (NullOrUndefined Nothing) }

-- | Constructs PutBucketReplicationRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newPutBucketReplicationRequest' :: BucketName -> ReplicationConfiguration -> ( { "Bucket" :: (BucketName) , "ContentMD5" :: NullOrUndefined (ContentMD5) , "ReplicationConfiguration" :: (ReplicationConfiguration) } -> {"Bucket" :: (BucketName) , "ContentMD5" :: NullOrUndefined (ContentMD5) , "ReplicationConfiguration" :: (ReplicationConfiguration) } ) -> PutBucketReplicationRequest
newPutBucketReplicationRequest' _Bucket _ReplicationConfiguration customize = (PutBucketReplicationRequest <<< customize) { "Bucket": _Bucket, "ReplicationConfiguration": _ReplicationConfiguration, "ContentMD5": (NullOrUndefined Nothing) }



newtype PutBucketRequestPaymentRequest = PutBucketRequestPaymentRequest 
  { "Bucket" :: (BucketName)
  , "ContentMD5" :: NullOrUndefined (ContentMD5)
  , "RequestPaymentConfiguration" :: (RequestPaymentConfiguration)
  }
derive instance newtypePutBucketRequestPaymentRequest :: Newtype PutBucketRequestPaymentRequest _
derive instance repGenericPutBucketRequestPaymentRequest :: Generic PutBucketRequestPaymentRequest _
instance showPutBucketRequestPaymentRequest :: Show PutBucketRequestPaymentRequest where show = genericShow
instance decodePutBucketRequestPaymentRequest :: Decode PutBucketRequestPaymentRequest where decode = genericDecode options
instance encodePutBucketRequestPaymentRequest :: Encode PutBucketRequestPaymentRequest where encode = genericEncode options

-- | Constructs PutBucketRequestPaymentRequest from required parameters
newPutBucketRequestPaymentRequest :: BucketName -> RequestPaymentConfiguration -> PutBucketRequestPaymentRequest
newPutBucketRequestPaymentRequest _Bucket _RequestPaymentConfiguration = PutBucketRequestPaymentRequest { "Bucket": _Bucket, "RequestPaymentConfiguration": _RequestPaymentConfiguration, "ContentMD5": (NullOrUndefined Nothing) }

-- | Constructs PutBucketRequestPaymentRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newPutBucketRequestPaymentRequest' :: BucketName -> RequestPaymentConfiguration -> ( { "Bucket" :: (BucketName) , "ContentMD5" :: NullOrUndefined (ContentMD5) , "RequestPaymentConfiguration" :: (RequestPaymentConfiguration) } -> {"Bucket" :: (BucketName) , "ContentMD5" :: NullOrUndefined (ContentMD5) , "RequestPaymentConfiguration" :: (RequestPaymentConfiguration) } ) -> PutBucketRequestPaymentRequest
newPutBucketRequestPaymentRequest' _Bucket _RequestPaymentConfiguration customize = (PutBucketRequestPaymentRequest <<< customize) { "Bucket": _Bucket, "RequestPaymentConfiguration": _RequestPaymentConfiguration, "ContentMD5": (NullOrUndefined Nothing) }



newtype PutBucketTaggingRequest = PutBucketTaggingRequest 
  { "Bucket" :: (BucketName)
  , "ContentMD5" :: NullOrUndefined (ContentMD5)
  , "Tagging" :: (Tagging)
  }
derive instance newtypePutBucketTaggingRequest :: Newtype PutBucketTaggingRequest _
derive instance repGenericPutBucketTaggingRequest :: Generic PutBucketTaggingRequest _
instance showPutBucketTaggingRequest :: Show PutBucketTaggingRequest where show = genericShow
instance decodePutBucketTaggingRequest :: Decode PutBucketTaggingRequest where decode = genericDecode options
instance encodePutBucketTaggingRequest :: Encode PutBucketTaggingRequest where encode = genericEncode options

-- | Constructs PutBucketTaggingRequest from required parameters
newPutBucketTaggingRequest :: BucketName -> Tagging -> PutBucketTaggingRequest
newPutBucketTaggingRequest _Bucket _Tagging = PutBucketTaggingRequest { "Bucket": _Bucket, "Tagging": _Tagging, "ContentMD5": (NullOrUndefined Nothing) }

-- | Constructs PutBucketTaggingRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newPutBucketTaggingRequest' :: BucketName -> Tagging -> ( { "Bucket" :: (BucketName) , "ContentMD5" :: NullOrUndefined (ContentMD5) , "Tagging" :: (Tagging) } -> {"Bucket" :: (BucketName) , "ContentMD5" :: NullOrUndefined (ContentMD5) , "Tagging" :: (Tagging) } ) -> PutBucketTaggingRequest
newPutBucketTaggingRequest' _Bucket _Tagging customize = (PutBucketTaggingRequest <<< customize) { "Bucket": _Bucket, "Tagging": _Tagging, "ContentMD5": (NullOrUndefined Nothing) }



newtype PutBucketVersioningRequest = PutBucketVersioningRequest 
  { "Bucket" :: (BucketName)
  , "ContentMD5" :: NullOrUndefined (ContentMD5)
  , "MFA" :: NullOrUndefined (MFA)
  , "VersioningConfiguration" :: (VersioningConfiguration)
  }
derive instance newtypePutBucketVersioningRequest :: Newtype PutBucketVersioningRequest _
derive instance repGenericPutBucketVersioningRequest :: Generic PutBucketVersioningRequest _
instance showPutBucketVersioningRequest :: Show PutBucketVersioningRequest where show = genericShow
instance decodePutBucketVersioningRequest :: Decode PutBucketVersioningRequest where decode = genericDecode options
instance encodePutBucketVersioningRequest :: Encode PutBucketVersioningRequest where encode = genericEncode options

-- | Constructs PutBucketVersioningRequest from required parameters
newPutBucketVersioningRequest :: BucketName -> VersioningConfiguration -> PutBucketVersioningRequest
newPutBucketVersioningRequest _Bucket _VersioningConfiguration = PutBucketVersioningRequest { "Bucket": _Bucket, "VersioningConfiguration": _VersioningConfiguration, "ContentMD5": (NullOrUndefined Nothing), "MFA": (NullOrUndefined Nothing) }

-- | Constructs PutBucketVersioningRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newPutBucketVersioningRequest' :: BucketName -> VersioningConfiguration -> ( { "Bucket" :: (BucketName) , "ContentMD5" :: NullOrUndefined (ContentMD5) , "MFA" :: NullOrUndefined (MFA) , "VersioningConfiguration" :: (VersioningConfiguration) } -> {"Bucket" :: (BucketName) , "ContentMD5" :: NullOrUndefined (ContentMD5) , "MFA" :: NullOrUndefined (MFA) , "VersioningConfiguration" :: (VersioningConfiguration) } ) -> PutBucketVersioningRequest
newPutBucketVersioningRequest' _Bucket _VersioningConfiguration customize = (PutBucketVersioningRequest <<< customize) { "Bucket": _Bucket, "VersioningConfiguration": _VersioningConfiguration, "ContentMD5": (NullOrUndefined Nothing), "MFA": (NullOrUndefined Nothing) }



newtype PutBucketWebsiteRequest = PutBucketWebsiteRequest 
  { "Bucket" :: (BucketName)
  , "ContentMD5" :: NullOrUndefined (ContentMD5)
  , "WebsiteConfiguration" :: (WebsiteConfiguration)
  }
derive instance newtypePutBucketWebsiteRequest :: Newtype PutBucketWebsiteRequest _
derive instance repGenericPutBucketWebsiteRequest :: Generic PutBucketWebsiteRequest _
instance showPutBucketWebsiteRequest :: Show PutBucketWebsiteRequest where show = genericShow
instance decodePutBucketWebsiteRequest :: Decode PutBucketWebsiteRequest where decode = genericDecode options
instance encodePutBucketWebsiteRequest :: Encode PutBucketWebsiteRequest where encode = genericEncode options

-- | Constructs PutBucketWebsiteRequest from required parameters
newPutBucketWebsiteRequest :: BucketName -> WebsiteConfiguration -> PutBucketWebsiteRequest
newPutBucketWebsiteRequest _Bucket _WebsiteConfiguration = PutBucketWebsiteRequest { "Bucket": _Bucket, "WebsiteConfiguration": _WebsiteConfiguration, "ContentMD5": (NullOrUndefined Nothing) }

-- | Constructs PutBucketWebsiteRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newPutBucketWebsiteRequest' :: BucketName -> WebsiteConfiguration -> ( { "Bucket" :: (BucketName) , "ContentMD5" :: NullOrUndefined (ContentMD5) , "WebsiteConfiguration" :: (WebsiteConfiguration) } -> {"Bucket" :: (BucketName) , "ContentMD5" :: NullOrUndefined (ContentMD5) , "WebsiteConfiguration" :: (WebsiteConfiguration) } ) -> PutBucketWebsiteRequest
newPutBucketWebsiteRequest' _Bucket _WebsiteConfiguration customize = (PutBucketWebsiteRequest <<< customize) { "Bucket": _Bucket, "WebsiteConfiguration": _WebsiteConfiguration, "ContentMD5": (NullOrUndefined Nothing) }



newtype PutObjectAclOutput = PutObjectAclOutput 
  { "RequestCharged" :: NullOrUndefined (RequestCharged)
  }
derive instance newtypePutObjectAclOutput :: Newtype PutObjectAclOutput _
derive instance repGenericPutObjectAclOutput :: Generic PutObjectAclOutput _
instance showPutObjectAclOutput :: Show PutObjectAclOutput where show = genericShow
instance decodePutObjectAclOutput :: Decode PutObjectAclOutput where decode = genericDecode options
instance encodePutObjectAclOutput :: Encode PutObjectAclOutput where encode = genericEncode options

-- | Constructs PutObjectAclOutput from required parameters
newPutObjectAclOutput :: PutObjectAclOutput
newPutObjectAclOutput  = PutObjectAclOutput { "RequestCharged": (NullOrUndefined Nothing) }

-- | Constructs PutObjectAclOutput's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newPutObjectAclOutput' :: ( { "RequestCharged" :: NullOrUndefined (RequestCharged) } -> {"RequestCharged" :: NullOrUndefined (RequestCharged) } ) -> PutObjectAclOutput
newPutObjectAclOutput'  customize = (PutObjectAclOutput <<< customize) { "RequestCharged": (NullOrUndefined Nothing) }



newtype PutObjectAclRequest = PutObjectAclRequest 
  { "ACL" :: NullOrUndefined (ObjectCannedACL)
  , "AccessControlPolicy" :: NullOrUndefined (AccessControlPolicy)
  , "Bucket" :: (BucketName)
  , "ContentMD5" :: NullOrUndefined (ContentMD5)
  , "GrantFullControl" :: NullOrUndefined (GrantFullControl)
  , "GrantRead" :: NullOrUndefined (GrantRead)
  , "GrantReadACP" :: NullOrUndefined (GrantReadACP)
  , "GrantWrite" :: NullOrUndefined (GrantWrite)
  , "GrantWriteACP" :: NullOrUndefined (GrantWriteACP)
  , "Key" :: (ObjectKey)
  , "RequestPayer" :: NullOrUndefined (RequestPayer)
  , "VersionId" :: NullOrUndefined (ObjectVersionId)
  }
derive instance newtypePutObjectAclRequest :: Newtype PutObjectAclRequest _
derive instance repGenericPutObjectAclRequest :: Generic PutObjectAclRequest _
instance showPutObjectAclRequest :: Show PutObjectAclRequest where show = genericShow
instance decodePutObjectAclRequest :: Decode PutObjectAclRequest where decode = genericDecode options
instance encodePutObjectAclRequest :: Encode PutObjectAclRequest where encode = genericEncode options

-- | Constructs PutObjectAclRequest from required parameters
newPutObjectAclRequest :: BucketName -> ObjectKey -> PutObjectAclRequest
newPutObjectAclRequest _Bucket _Key = PutObjectAclRequest { "Bucket": _Bucket, "Key": _Key, "ACL": (NullOrUndefined Nothing), "AccessControlPolicy": (NullOrUndefined Nothing), "ContentMD5": (NullOrUndefined Nothing), "GrantFullControl": (NullOrUndefined Nothing), "GrantRead": (NullOrUndefined Nothing), "GrantReadACP": (NullOrUndefined Nothing), "GrantWrite": (NullOrUndefined Nothing), "GrantWriteACP": (NullOrUndefined Nothing), "RequestPayer": (NullOrUndefined Nothing), "VersionId": (NullOrUndefined Nothing) }

-- | Constructs PutObjectAclRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newPutObjectAclRequest' :: BucketName -> ObjectKey -> ( { "ACL" :: NullOrUndefined (ObjectCannedACL) , "AccessControlPolicy" :: NullOrUndefined (AccessControlPolicy) , "Bucket" :: (BucketName) , "ContentMD5" :: NullOrUndefined (ContentMD5) , "GrantFullControl" :: NullOrUndefined (GrantFullControl) , "GrantRead" :: NullOrUndefined (GrantRead) , "GrantReadACP" :: NullOrUndefined (GrantReadACP) , "GrantWrite" :: NullOrUndefined (GrantWrite) , "GrantWriteACP" :: NullOrUndefined (GrantWriteACP) , "Key" :: (ObjectKey) , "RequestPayer" :: NullOrUndefined (RequestPayer) , "VersionId" :: NullOrUndefined (ObjectVersionId) } -> {"ACL" :: NullOrUndefined (ObjectCannedACL) , "AccessControlPolicy" :: NullOrUndefined (AccessControlPolicy) , "Bucket" :: (BucketName) , "ContentMD5" :: NullOrUndefined (ContentMD5) , "GrantFullControl" :: NullOrUndefined (GrantFullControl) , "GrantRead" :: NullOrUndefined (GrantRead) , "GrantReadACP" :: NullOrUndefined (GrantReadACP) , "GrantWrite" :: NullOrUndefined (GrantWrite) , "GrantWriteACP" :: NullOrUndefined (GrantWriteACP) , "Key" :: (ObjectKey) , "RequestPayer" :: NullOrUndefined (RequestPayer) , "VersionId" :: NullOrUndefined (ObjectVersionId) } ) -> PutObjectAclRequest
newPutObjectAclRequest' _Bucket _Key customize = (PutObjectAclRequest <<< customize) { "Bucket": _Bucket, "Key": _Key, "ACL": (NullOrUndefined Nothing), "AccessControlPolicy": (NullOrUndefined Nothing), "ContentMD5": (NullOrUndefined Nothing), "GrantFullControl": (NullOrUndefined Nothing), "GrantRead": (NullOrUndefined Nothing), "GrantReadACP": (NullOrUndefined Nothing), "GrantWrite": (NullOrUndefined Nothing), "GrantWriteACP": (NullOrUndefined Nothing), "RequestPayer": (NullOrUndefined Nothing), "VersionId": (NullOrUndefined Nothing) }



newtype PutObjectOutput = PutObjectOutput 
  { "Expiration" :: NullOrUndefined (Expiration)
  , "ETag" :: NullOrUndefined (ETag)
  , "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption)
  , "VersionId" :: NullOrUndefined (ObjectVersionId)
  , "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm)
  , "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5)
  , "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId)
  , "RequestCharged" :: NullOrUndefined (RequestCharged)
  }
derive instance newtypePutObjectOutput :: Newtype PutObjectOutput _
derive instance repGenericPutObjectOutput :: Generic PutObjectOutput _
instance showPutObjectOutput :: Show PutObjectOutput where show = genericShow
instance decodePutObjectOutput :: Decode PutObjectOutput where decode = genericDecode options
instance encodePutObjectOutput :: Encode PutObjectOutput where encode = genericEncode options

-- | Constructs PutObjectOutput from required parameters
newPutObjectOutput :: PutObjectOutput
newPutObjectOutput  = PutObjectOutput { "ETag": (NullOrUndefined Nothing), "Expiration": (NullOrUndefined Nothing), "RequestCharged": (NullOrUndefined Nothing), "SSECustomerAlgorithm": (NullOrUndefined Nothing), "SSECustomerKeyMD5": (NullOrUndefined Nothing), "SSEKMSKeyId": (NullOrUndefined Nothing), "ServerSideEncryption": (NullOrUndefined Nothing), "VersionId": (NullOrUndefined Nothing) }

-- | Constructs PutObjectOutput's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newPutObjectOutput' :: ( { "Expiration" :: NullOrUndefined (Expiration) , "ETag" :: NullOrUndefined (ETag) , "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption) , "VersionId" :: NullOrUndefined (ObjectVersionId) , "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm) , "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5) , "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId) , "RequestCharged" :: NullOrUndefined (RequestCharged) } -> {"Expiration" :: NullOrUndefined (Expiration) , "ETag" :: NullOrUndefined (ETag) , "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption) , "VersionId" :: NullOrUndefined (ObjectVersionId) , "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm) , "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5) , "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId) , "RequestCharged" :: NullOrUndefined (RequestCharged) } ) -> PutObjectOutput
newPutObjectOutput'  customize = (PutObjectOutput <<< customize) { "ETag": (NullOrUndefined Nothing), "Expiration": (NullOrUndefined Nothing), "RequestCharged": (NullOrUndefined Nothing), "SSECustomerAlgorithm": (NullOrUndefined Nothing), "SSECustomerKeyMD5": (NullOrUndefined Nothing), "SSEKMSKeyId": (NullOrUndefined Nothing), "ServerSideEncryption": (NullOrUndefined Nothing), "VersionId": (NullOrUndefined Nothing) }



newtype PutObjectRequest = PutObjectRequest 
  { "ACL" :: NullOrUndefined (ObjectCannedACL)
  , "Body" :: NullOrUndefined (Body)
  , "Bucket" :: (BucketName)
  , "CacheControl" :: NullOrUndefined (CacheControl)
  , "ContentDisposition" :: NullOrUndefined (ContentDisposition)
  , "ContentEncoding" :: NullOrUndefined (ContentEncoding)
  , "ContentLanguage" :: NullOrUndefined (ContentLanguage)
  , "ContentLength" :: NullOrUndefined (ContentLength)
  , "ContentMD5" :: NullOrUndefined (ContentMD5)
  , "ContentType" :: NullOrUndefined (ContentType)
  , "Expires" :: NullOrUndefined (Expires)
  , "GrantFullControl" :: NullOrUndefined (GrantFullControl)
  , "GrantRead" :: NullOrUndefined (GrantRead)
  , "GrantReadACP" :: NullOrUndefined (GrantReadACP)
  , "GrantWriteACP" :: NullOrUndefined (GrantWriteACP)
  , "Key" :: (ObjectKey)
  , "Metadata" :: NullOrUndefined (Metadata)
  , "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption)
  , "StorageClass" :: NullOrUndefined (StorageClass)
  , "WebsiteRedirectLocation" :: NullOrUndefined (WebsiteRedirectLocation)
  , "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm)
  , "SSECustomerKey" :: NullOrUndefined (SSECustomerKey)
  , "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5)
  , "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId)
  , "RequestPayer" :: NullOrUndefined (RequestPayer)
  , "Tagging" :: NullOrUndefined (TaggingHeader)
  }
derive instance newtypePutObjectRequest :: Newtype PutObjectRequest _
derive instance repGenericPutObjectRequest :: Generic PutObjectRequest _
instance showPutObjectRequest :: Show PutObjectRequest where show = genericShow
instance decodePutObjectRequest :: Decode PutObjectRequest where decode = genericDecode options
instance encodePutObjectRequest :: Encode PutObjectRequest where encode = genericEncode options

-- | Constructs PutObjectRequest from required parameters
newPutObjectRequest :: BucketName -> ObjectKey -> PutObjectRequest
newPutObjectRequest _Bucket _Key = PutObjectRequest { "Bucket": _Bucket, "Key": _Key, "ACL": (NullOrUndefined Nothing), "Body": (NullOrUndefined Nothing), "CacheControl": (NullOrUndefined Nothing), "ContentDisposition": (NullOrUndefined Nothing), "ContentEncoding": (NullOrUndefined Nothing), "ContentLanguage": (NullOrUndefined Nothing), "ContentLength": (NullOrUndefined Nothing), "ContentMD5": (NullOrUndefined Nothing), "ContentType": (NullOrUndefined Nothing), "Expires": (NullOrUndefined Nothing), "GrantFullControl": (NullOrUndefined Nothing), "GrantRead": (NullOrUndefined Nothing), "GrantReadACP": (NullOrUndefined Nothing), "GrantWriteACP": (NullOrUndefined Nothing), "Metadata": (NullOrUndefined Nothing), "RequestPayer": (NullOrUndefined Nothing), "SSECustomerAlgorithm": (NullOrUndefined Nothing), "SSECustomerKey": (NullOrUndefined Nothing), "SSECustomerKeyMD5": (NullOrUndefined Nothing), "SSEKMSKeyId": (NullOrUndefined Nothing), "ServerSideEncryption": (NullOrUndefined Nothing), "StorageClass": (NullOrUndefined Nothing), "Tagging": (NullOrUndefined Nothing), "WebsiteRedirectLocation": (NullOrUndefined Nothing) }

-- | Constructs PutObjectRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newPutObjectRequest' :: BucketName -> ObjectKey -> ( { "ACL" :: NullOrUndefined (ObjectCannedACL) , "Body" :: NullOrUndefined (Body) , "Bucket" :: (BucketName) , "CacheControl" :: NullOrUndefined (CacheControl) , "ContentDisposition" :: NullOrUndefined (ContentDisposition) , "ContentEncoding" :: NullOrUndefined (ContentEncoding) , "ContentLanguage" :: NullOrUndefined (ContentLanguage) , "ContentLength" :: NullOrUndefined (ContentLength) , "ContentMD5" :: NullOrUndefined (ContentMD5) , "ContentType" :: NullOrUndefined (ContentType) , "Expires" :: NullOrUndefined (Expires) , "GrantFullControl" :: NullOrUndefined (GrantFullControl) , "GrantRead" :: NullOrUndefined (GrantRead) , "GrantReadACP" :: NullOrUndefined (GrantReadACP) , "GrantWriteACP" :: NullOrUndefined (GrantWriteACP) , "Key" :: (ObjectKey) , "Metadata" :: NullOrUndefined (Metadata) , "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption) , "StorageClass" :: NullOrUndefined (StorageClass) , "WebsiteRedirectLocation" :: NullOrUndefined (WebsiteRedirectLocation) , "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm) , "SSECustomerKey" :: NullOrUndefined (SSECustomerKey) , "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5) , "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId) , "RequestPayer" :: NullOrUndefined (RequestPayer) , "Tagging" :: NullOrUndefined (TaggingHeader) } -> {"ACL" :: NullOrUndefined (ObjectCannedACL) , "Body" :: NullOrUndefined (Body) , "Bucket" :: (BucketName) , "CacheControl" :: NullOrUndefined (CacheControl) , "ContentDisposition" :: NullOrUndefined (ContentDisposition) , "ContentEncoding" :: NullOrUndefined (ContentEncoding) , "ContentLanguage" :: NullOrUndefined (ContentLanguage) , "ContentLength" :: NullOrUndefined (ContentLength) , "ContentMD5" :: NullOrUndefined (ContentMD5) , "ContentType" :: NullOrUndefined (ContentType) , "Expires" :: NullOrUndefined (Expires) , "GrantFullControl" :: NullOrUndefined (GrantFullControl) , "GrantRead" :: NullOrUndefined (GrantRead) , "GrantReadACP" :: NullOrUndefined (GrantReadACP) , "GrantWriteACP" :: NullOrUndefined (GrantWriteACP) , "Key" :: (ObjectKey) , "Metadata" :: NullOrUndefined (Metadata) , "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption) , "StorageClass" :: NullOrUndefined (StorageClass) , "WebsiteRedirectLocation" :: NullOrUndefined (WebsiteRedirectLocation) , "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm) , "SSECustomerKey" :: NullOrUndefined (SSECustomerKey) , "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5) , "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId) , "RequestPayer" :: NullOrUndefined (RequestPayer) , "Tagging" :: NullOrUndefined (TaggingHeader) } ) -> PutObjectRequest
newPutObjectRequest' _Bucket _Key customize = (PutObjectRequest <<< customize) { "Bucket": _Bucket, "Key": _Key, "ACL": (NullOrUndefined Nothing), "Body": (NullOrUndefined Nothing), "CacheControl": (NullOrUndefined Nothing), "ContentDisposition": (NullOrUndefined Nothing), "ContentEncoding": (NullOrUndefined Nothing), "ContentLanguage": (NullOrUndefined Nothing), "ContentLength": (NullOrUndefined Nothing), "ContentMD5": (NullOrUndefined Nothing), "ContentType": (NullOrUndefined Nothing), "Expires": (NullOrUndefined Nothing), "GrantFullControl": (NullOrUndefined Nothing), "GrantRead": (NullOrUndefined Nothing), "GrantReadACP": (NullOrUndefined Nothing), "GrantWriteACP": (NullOrUndefined Nothing), "Metadata": (NullOrUndefined Nothing), "RequestPayer": (NullOrUndefined Nothing), "SSECustomerAlgorithm": (NullOrUndefined Nothing), "SSECustomerKey": (NullOrUndefined Nothing), "SSECustomerKeyMD5": (NullOrUndefined Nothing), "SSEKMSKeyId": (NullOrUndefined Nothing), "ServerSideEncryption": (NullOrUndefined Nothing), "StorageClass": (NullOrUndefined Nothing), "Tagging": (NullOrUndefined Nothing), "WebsiteRedirectLocation": (NullOrUndefined Nothing) }



newtype PutObjectTaggingOutput = PutObjectTaggingOutput 
  { "VersionId" :: NullOrUndefined (ObjectVersionId)
  }
derive instance newtypePutObjectTaggingOutput :: Newtype PutObjectTaggingOutput _
derive instance repGenericPutObjectTaggingOutput :: Generic PutObjectTaggingOutput _
instance showPutObjectTaggingOutput :: Show PutObjectTaggingOutput where show = genericShow
instance decodePutObjectTaggingOutput :: Decode PutObjectTaggingOutput where decode = genericDecode options
instance encodePutObjectTaggingOutput :: Encode PutObjectTaggingOutput where encode = genericEncode options

-- | Constructs PutObjectTaggingOutput from required parameters
newPutObjectTaggingOutput :: PutObjectTaggingOutput
newPutObjectTaggingOutput  = PutObjectTaggingOutput { "VersionId": (NullOrUndefined Nothing) }

-- | Constructs PutObjectTaggingOutput's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newPutObjectTaggingOutput' :: ( { "VersionId" :: NullOrUndefined (ObjectVersionId) } -> {"VersionId" :: NullOrUndefined (ObjectVersionId) } ) -> PutObjectTaggingOutput
newPutObjectTaggingOutput'  customize = (PutObjectTaggingOutput <<< customize) { "VersionId": (NullOrUndefined Nothing) }



newtype PutObjectTaggingRequest = PutObjectTaggingRequest 
  { "Bucket" :: (BucketName)
  , "Key" :: (ObjectKey)
  , "VersionId" :: NullOrUndefined (ObjectVersionId)
  , "ContentMD5" :: NullOrUndefined (ContentMD5)
  , "Tagging" :: (Tagging)
  }
derive instance newtypePutObjectTaggingRequest :: Newtype PutObjectTaggingRequest _
derive instance repGenericPutObjectTaggingRequest :: Generic PutObjectTaggingRequest _
instance showPutObjectTaggingRequest :: Show PutObjectTaggingRequest where show = genericShow
instance decodePutObjectTaggingRequest :: Decode PutObjectTaggingRequest where decode = genericDecode options
instance encodePutObjectTaggingRequest :: Encode PutObjectTaggingRequest where encode = genericEncode options

-- | Constructs PutObjectTaggingRequest from required parameters
newPutObjectTaggingRequest :: BucketName -> ObjectKey -> Tagging -> PutObjectTaggingRequest
newPutObjectTaggingRequest _Bucket _Key _Tagging = PutObjectTaggingRequest { "Bucket": _Bucket, "Key": _Key, "Tagging": _Tagging, "ContentMD5": (NullOrUndefined Nothing), "VersionId": (NullOrUndefined Nothing) }

-- | Constructs PutObjectTaggingRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newPutObjectTaggingRequest' :: BucketName -> ObjectKey -> Tagging -> ( { "Bucket" :: (BucketName) , "Key" :: (ObjectKey) , "VersionId" :: NullOrUndefined (ObjectVersionId) , "ContentMD5" :: NullOrUndefined (ContentMD5) , "Tagging" :: (Tagging) } -> {"Bucket" :: (BucketName) , "Key" :: (ObjectKey) , "VersionId" :: NullOrUndefined (ObjectVersionId) , "ContentMD5" :: NullOrUndefined (ContentMD5) , "Tagging" :: (Tagging) } ) -> PutObjectTaggingRequest
newPutObjectTaggingRequest' _Bucket _Key _Tagging customize = (PutObjectTaggingRequest <<< customize) { "Bucket": _Bucket, "Key": _Key, "Tagging": _Tagging, "ContentMD5": (NullOrUndefined Nothing), "VersionId": (NullOrUndefined Nothing) }



newtype QueueArn = QueueArn String
derive instance newtypeQueueArn :: Newtype QueueArn _
derive instance repGenericQueueArn :: Generic QueueArn _
instance showQueueArn :: Show QueueArn where show = genericShow
instance decodeQueueArn :: Decode QueueArn where decode = genericDecode options
instance encodeQueueArn :: Encode QueueArn where encode = genericEncode options



-- | Container for specifying an configuration when you want Amazon S3 to publish events to an Amazon Simple Queue Service (Amazon SQS) queue.
newtype QueueConfiguration = QueueConfiguration 
  { "Id" :: NullOrUndefined (NotificationId)
  , "QueueArn" :: (QueueArn)
  , "Events" :: (EventList)
  , "Filter" :: NullOrUndefined (NotificationConfigurationFilter)
  }
derive instance newtypeQueueConfiguration :: Newtype QueueConfiguration _
derive instance repGenericQueueConfiguration :: Generic QueueConfiguration _
instance showQueueConfiguration :: Show QueueConfiguration where show = genericShow
instance decodeQueueConfiguration :: Decode QueueConfiguration where decode = genericDecode options
instance encodeQueueConfiguration :: Encode QueueConfiguration where encode = genericEncode options

-- | Constructs QueueConfiguration from required parameters
newQueueConfiguration :: EventList -> QueueArn -> QueueConfiguration
newQueueConfiguration _Events _QueueArn = QueueConfiguration { "Events": _Events, "QueueArn": _QueueArn, "Filter": (NullOrUndefined Nothing), "Id": (NullOrUndefined Nothing) }

-- | Constructs QueueConfiguration's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newQueueConfiguration' :: EventList -> QueueArn -> ( { "Id" :: NullOrUndefined (NotificationId) , "QueueArn" :: (QueueArn) , "Events" :: (EventList) , "Filter" :: NullOrUndefined (NotificationConfigurationFilter) } -> {"Id" :: NullOrUndefined (NotificationId) , "QueueArn" :: (QueueArn) , "Events" :: (EventList) , "Filter" :: NullOrUndefined (NotificationConfigurationFilter) } ) -> QueueConfiguration
newQueueConfiguration' _Events _QueueArn customize = (QueueConfiguration <<< customize) { "Events": _Events, "QueueArn": _QueueArn, "Filter": (NullOrUndefined Nothing), "Id": (NullOrUndefined Nothing) }



newtype QueueConfigurationDeprecated = QueueConfigurationDeprecated 
  { "Id" :: NullOrUndefined (NotificationId)
  , "Event" :: NullOrUndefined (Event)
  , "Events" :: NullOrUndefined (EventList)
  , "Queue" :: NullOrUndefined (QueueArn)
  }
derive instance newtypeQueueConfigurationDeprecated :: Newtype QueueConfigurationDeprecated _
derive instance repGenericQueueConfigurationDeprecated :: Generic QueueConfigurationDeprecated _
instance showQueueConfigurationDeprecated :: Show QueueConfigurationDeprecated where show = genericShow
instance decodeQueueConfigurationDeprecated :: Decode QueueConfigurationDeprecated where decode = genericDecode options
instance encodeQueueConfigurationDeprecated :: Encode QueueConfigurationDeprecated where encode = genericEncode options

-- | Constructs QueueConfigurationDeprecated from required parameters
newQueueConfigurationDeprecated :: QueueConfigurationDeprecated
newQueueConfigurationDeprecated  = QueueConfigurationDeprecated { "Event": (NullOrUndefined Nothing), "Events": (NullOrUndefined Nothing), "Id": (NullOrUndefined Nothing), "Queue": (NullOrUndefined Nothing) }

-- | Constructs QueueConfigurationDeprecated's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newQueueConfigurationDeprecated' :: ( { "Id" :: NullOrUndefined (NotificationId) , "Event" :: NullOrUndefined (Event) , "Events" :: NullOrUndefined (EventList) , "Queue" :: NullOrUndefined (QueueArn) } -> {"Id" :: NullOrUndefined (NotificationId) , "Event" :: NullOrUndefined (Event) , "Events" :: NullOrUndefined (EventList) , "Queue" :: NullOrUndefined (QueueArn) } ) -> QueueConfigurationDeprecated
newQueueConfigurationDeprecated'  customize = (QueueConfigurationDeprecated <<< customize) { "Event": (NullOrUndefined Nothing), "Events": (NullOrUndefined Nothing), "Id": (NullOrUndefined Nothing), "Queue": (NullOrUndefined Nothing) }



newtype QueueConfigurationList = QueueConfigurationList (Array QueueConfiguration)
derive instance newtypeQueueConfigurationList :: Newtype QueueConfigurationList _
derive instance repGenericQueueConfigurationList :: Generic QueueConfigurationList _
instance showQueueConfigurationList :: Show QueueConfigurationList where show = genericShow
instance decodeQueueConfigurationList :: Decode QueueConfigurationList where decode = genericDecode options
instance encodeQueueConfigurationList :: Encode QueueConfigurationList where encode = genericEncode options



newtype Quiet = Quiet Boolean
derive instance newtypeQuiet :: Newtype Quiet _
derive instance repGenericQuiet :: Generic Quiet _
instance showQuiet :: Show Quiet where show = genericShow
instance decodeQuiet :: Decode Quiet where decode = genericDecode options
instance encodeQuiet :: Encode Quiet where encode = genericEncode options



newtype QuoteCharacter = QuoteCharacter String
derive instance newtypeQuoteCharacter :: Newtype QuoteCharacter _
derive instance repGenericQuoteCharacter :: Generic QuoteCharacter _
instance showQuoteCharacter :: Show QuoteCharacter where show = genericShow
instance decodeQuoteCharacter :: Decode QuoteCharacter where decode = genericDecode options
instance encodeQuoteCharacter :: Encode QuoteCharacter where encode = genericEncode options



newtype QuoteEscapeCharacter = QuoteEscapeCharacter String
derive instance newtypeQuoteEscapeCharacter :: Newtype QuoteEscapeCharacter _
derive instance repGenericQuoteEscapeCharacter :: Generic QuoteEscapeCharacter _
instance showQuoteEscapeCharacter :: Show QuoteEscapeCharacter where show = genericShow
instance decodeQuoteEscapeCharacter :: Decode QuoteEscapeCharacter where decode = genericDecode options
instance encodeQuoteEscapeCharacter :: Encode QuoteEscapeCharacter where encode = genericEncode options



newtype QuoteFields = QuoteFields String
derive instance newtypeQuoteFields :: Newtype QuoteFields _
derive instance repGenericQuoteFields :: Generic QuoteFields _
instance showQuoteFields :: Show QuoteFields where show = genericShow
instance decodeQuoteFields :: Decode QuoteFields where decode = genericDecode options
instance encodeQuoteFields :: Encode QuoteFields where encode = genericEncode options



newtype Range = Range String
derive instance newtypeRange :: Newtype Range _
derive instance repGenericRange :: Generic Range _
instance showRange :: Show Range where show = genericShow
instance decodeRange :: Decode Range where decode = genericDecode options
instance encodeRange :: Encode Range where encode = genericEncode options



newtype RecordDelimiter = RecordDelimiter String
derive instance newtypeRecordDelimiter :: Newtype RecordDelimiter _
derive instance repGenericRecordDelimiter :: Generic RecordDelimiter _
instance showRecordDelimiter :: Show RecordDelimiter where show = genericShow
instance decodeRecordDelimiter :: Decode RecordDelimiter where decode = genericDecode options
instance encodeRecordDelimiter :: Encode RecordDelimiter where encode = genericEncode options



newtype Redirect = Redirect 
  { "HostName" :: NullOrUndefined (HostName)
  , "HttpRedirectCode" :: NullOrUndefined (HttpRedirectCode)
  , "Protocol" :: NullOrUndefined (Protocol)
  , "ReplaceKeyPrefixWith" :: NullOrUndefined (ReplaceKeyPrefixWith)
  , "ReplaceKeyWith" :: NullOrUndefined (ReplaceKeyWith)
  }
derive instance newtypeRedirect :: Newtype Redirect _
derive instance repGenericRedirect :: Generic Redirect _
instance showRedirect :: Show Redirect where show = genericShow
instance decodeRedirect :: Decode Redirect where decode = genericDecode options
instance encodeRedirect :: Encode Redirect where encode = genericEncode options

-- | Constructs Redirect from required parameters
newRedirect :: Redirect
newRedirect  = Redirect { "HostName": (NullOrUndefined Nothing), "HttpRedirectCode": (NullOrUndefined Nothing), "Protocol": (NullOrUndefined Nothing), "ReplaceKeyPrefixWith": (NullOrUndefined Nothing), "ReplaceKeyWith": (NullOrUndefined Nothing) }

-- | Constructs Redirect's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newRedirect' :: ( { "HostName" :: NullOrUndefined (HostName) , "HttpRedirectCode" :: NullOrUndefined (HttpRedirectCode) , "Protocol" :: NullOrUndefined (Protocol) , "ReplaceKeyPrefixWith" :: NullOrUndefined (ReplaceKeyPrefixWith) , "ReplaceKeyWith" :: NullOrUndefined (ReplaceKeyWith) } -> {"HostName" :: NullOrUndefined (HostName) , "HttpRedirectCode" :: NullOrUndefined (HttpRedirectCode) , "Protocol" :: NullOrUndefined (Protocol) , "ReplaceKeyPrefixWith" :: NullOrUndefined (ReplaceKeyPrefixWith) , "ReplaceKeyWith" :: NullOrUndefined (ReplaceKeyWith) } ) -> Redirect
newRedirect'  customize = (Redirect <<< customize) { "HostName": (NullOrUndefined Nothing), "HttpRedirectCode": (NullOrUndefined Nothing), "Protocol": (NullOrUndefined Nothing), "ReplaceKeyPrefixWith": (NullOrUndefined Nothing), "ReplaceKeyWith": (NullOrUndefined Nothing) }



newtype RedirectAllRequestsTo = RedirectAllRequestsTo 
  { "HostName" :: (HostName)
  , "Protocol" :: NullOrUndefined (Protocol)
  }
derive instance newtypeRedirectAllRequestsTo :: Newtype RedirectAllRequestsTo _
derive instance repGenericRedirectAllRequestsTo :: Generic RedirectAllRequestsTo _
instance showRedirectAllRequestsTo :: Show RedirectAllRequestsTo where show = genericShow
instance decodeRedirectAllRequestsTo :: Decode RedirectAllRequestsTo where decode = genericDecode options
instance encodeRedirectAllRequestsTo :: Encode RedirectAllRequestsTo where encode = genericEncode options

-- | Constructs RedirectAllRequestsTo from required parameters
newRedirectAllRequestsTo :: HostName -> RedirectAllRequestsTo
newRedirectAllRequestsTo _HostName = RedirectAllRequestsTo { "HostName": _HostName, "Protocol": (NullOrUndefined Nothing) }

-- | Constructs RedirectAllRequestsTo's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newRedirectAllRequestsTo' :: HostName -> ( { "HostName" :: (HostName) , "Protocol" :: NullOrUndefined (Protocol) } -> {"HostName" :: (HostName) , "Protocol" :: NullOrUndefined (Protocol) } ) -> RedirectAllRequestsTo
newRedirectAllRequestsTo' _HostName customize = (RedirectAllRequestsTo <<< customize) { "HostName": _HostName, "Protocol": (NullOrUndefined Nothing) }



newtype ReplaceKeyPrefixWith = ReplaceKeyPrefixWith String
derive instance newtypeReplaceKeyPrefixWith :: Newtype ReplaceKeyPrefixWith _
derive instance repGenericReplaceKeyPrefixWith :: Generic ReplaceKeyPrefixWith _
instance showReplaceKeyPrefixWith :: Show ReplaceKeyPrefixWith where show = genericShow
instance decodeReplaceKeyPrefixWith :: Decode ReplaceKeyPrefixWith where decode = genericDecode options
instance encodeReplaceKeyPrefixWith :: Encode ReplaceKeyPrefixWith where encode = genericEncode options



newtype ReplaceKeyWith = ReplaceKeyWith String
derive instance newtypeReplaceKeyWith :: Newtype ReplaceKeyWith _
derive instance repGenericReplaceKeyWith :: Generic ReplaceKeyWith _
instance showReplaceKeyWith :: Show ReplaceKeyWith where show = genericShow
instance decodeReplaceKeyWith :: Decode ReplaceKeyWith where decode = genericDecode options
instance encodeReplaceKeyWith :: Encode ReplaceKeyWith where encode = genericEncode options



newtype ReplicaKmsKeyID = ReplicaKmsKeyID String
derive instance newtypeReplicaKmsKeyID :: Newtype ReplicaKmsKeyID _
derive instance repGenericReplicaKmsKeyID :: Generic ReplicaKmsKeyID _
instance showReplicaKmsKeyID :: Show ReplicaKmsKeyID where show = genericShow
instance decodeReplicaKmsKeyID :: Decode ReplicaKmsKeyID where decode = genericDecode options
instance encodeReplicaKmsKeyID :: Encode ReplicaKmsKeyID where encode = genericEncode options



-- | Container for replication rules. You can add as many as 1,000 rules. Total replication configuration size can be up to 2 MB.
newtype ReplicationConfiguration = ReplicationConfiguration 
  { "Role" :: (Role)
  , "Rules" :: (ReplicationRules)
  }
derive instance newtypeReplicationConfiguration :: Newtype ReplicationConfiguration _
derive instance repGenericReplicationConfiguration :: Generic ReplicationConfiguration _
instance showReplicationConfiguration :: Show ReplicationConfiguration where show = genericShow
instance decodeReplicationConfiguration :: Decode ReplicationConfiguration where decode = genericDecode options
instance encodeReplicationConfiguration :: Encode ReplicationConfiguration where encode = genericEncode options

-- | Constructs ReplicationConfiguration from required parameters
newReplicationConfiguration :: Role -> ReplicationRules -> ReplicationConfiguration
newReplicationConfiguration _Role _Rules = ReplicationConfiguration { "Role": _Role, "Rules": _Rules }

-- | Constructs ReplicationConfiguration's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newReplicationConfiguration' :: Role -> ReplicationRules -> ( { "Role" :: (Role) , "Rules" :: (ReplicationRules) } -> {"Role" :: (Role) , "Rules" :: (ReplicationRules) } ) -> ReplicationConfiguration
newReplicationConfiguration' _Role _Rules customize = (ReplicationConfiguration <<< customize) { "Role": _Role, "Rules": _Rules }



-- | Container for information about a particular replication rule.
newtype ReplicationRule = ReplicationRule 
  { "ID" :: NullOrUndefined (ID)
  , "Prefix" :: (Prefix)
  , "Status" :: (ReplicationRuleStatus)
  , "SourceSelectionCriteria" :: NullOrUndefined (SourceSelectionCriteria)
  , "Destination" :: (Destination)
  }
derive instance newtypeReplicationRule :: Newtype ReplicationRule _
derive instance repGenericReplicationRule :: Generic ReplicationRule _
instance showReplicationRule :: Show ReplicationRule where show = genericShow
instance decodeReplicationRule :: Decode ReplicationRule where decode = genericDecode options
instance encodeReplicationRule :: Encode ReplicationRule where encode = genericEncode options

-- | Constructs ReplicationRule from required parameters
newReplicationRule :: Destination -> Prefix -> ReplicationRuleStatus -> ReplicationRule
newReplicationRule _Destination _Prefix _Status = ReplicationRule { "Destination": _Destination, "Prefix": _Prefix, "Status": _Status, "ID": (NullOrUndefined Nothing), "SourceSelectionCriteria": (NullOrUndefined Nothing) }

-- | Constructs ReplicationRule's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newReplicationRule' :: Destination -> Prefix -> ReplicationRuleStatus -> ( { "ID" :: NullOrUndefined (ID) , "Prefix" :: (Prefix) , "Status" :: (ReplicationRuleStatus) , "SourceSelectionCriteria" :: NullOrUndefined (SourceSelectionCriteria) , "Destination" :: (Destination) } -> {"ID" :: NullOrUndefined (ID) , "Prefix" :: (Prefix) , "Status" :: (ReplicationRuleStatus) , "SourceSelectionCriteria" :: NullOrUndefined (SourceSelectionCriteria) , "Destination" :: (Destination) } ) -> ReplicationRule
newReplicationRule' _Destination _Prefix _Status customize = (ReplicationRule <<< customize) { "Destination": _Destination, "Prefix": _Prefix, "Status": _Status, "ID": (NullOrUndefined Nothing), "SourceSelectionCriteria": (NullOrUndefined Nothing) }



newtype ReplicationRuleStatus = ReplicationRuleStatus String
derive instance newtypeReplicationRuleStatus :: Newtype ReplicationRuleStatus _
derive instance repGenericReplicationRuleStatus :: Generic ReplicationRuleStatus _
instance showReplicationRuleStatus :: Show ReplicationRuleStatus where show = genericShow
instance decodeReplicationRuleStatus :: Decode ReplicationRuleStatus where decode = genericDecode options
instance encodeReplicationRuleStatus :: Encode ReplicationRuleStatus where encode = genericEncode options



newtype ReplicationRules = ReplicationRules (Array ReplicationRule)
derive instance newtypeReplicationRules :: Newtype ReplicationRules _
derive instance repGenericReplicationRules :: Generic ReplicationRules _
instance showReplicationRules :: Show ReplicationRules where show = genericShow
instance decodeReplicationRules :: Decode ReplicationRules where decode = genericDecode options
instance encodeReplicationRules :: Encode ReplicationRules where encode = genericEncode options



newtype ReplicationStatus = ReplicationStatus String
derive instance newtypeReplicationStatus :: Newtype ReplicationStatus _
derive instance repGenericReplicationStatus :: Generic ReplicationStatus _
instance showReplicationStatus :: Show ReplicationStatus where show = genericShow
instance decodeReplicationStatus :: Decode ReplicationStatus where decode = genericDecode options
instance encodeReplicationStatus :: Encode ReplicationStatus where encode = genericEncode options



-- | If present, indicates that the requester was successfully charged for the request.
newtype RequestCharged = RequestCharged String
derive instance newtypeRequestCharged :: Newtype RequestCharged _
derive instance repGenericRequestCharged :: Generic RequestCharged _
instance showRequestCharged :: Show RequestCharged where show = genericShow
instance decodeRequestCharged :: Decode RequestCharged where decode = genericDecode options
instance encodeRequestCharged :: Encode RequestCharged where encode = genericEncode options



-- | Confirms that the requester knows that she or he will be charged for the request. Bucket owners need not specify this parameter in their requests. Documentation on downloading objects from requester pays buckets can be found at http://docs.aws.amazon.com/AmazonS3/latest/dev/ObjectsinRequesterPaysBuckets.html
newtype RequestPayer = RequestPayer String
derive instance newtypeRequestPayer :: Newtype RequestPayer _
derive instance repGenericRequestPayer :: Generic RequestPayer _
instance showRequestPayer :: Show RequestPayer where show = genericShow
instance decodeRequestPayer :: Decode RequestPayer where decode = genericDecode options
instance encodeRequestPayer :: Encode RequestPayer where encode = genericEncode options



newtype RequestPaymentConfiguration = RequestPaymentConfiguration 
  { "Payer" :: (Payer)
  }
derive instance newtypeRequestPaymentConfiguration :: Newtype RequestPaymentConfiguration _
derive instance repGenericRequestPaymentConfiguration :: Generic RequestPaymentConfiguration _
instance showRequestPaymentConfiguration :: Show RequestPaymentConfiguration where show = genericShow
instance decodeRequestPaymentConfiguration :: Decode RequestPaymentConfiguration where decode = genericDecode options
instance encodeRequestPaymentConfiguration :: Encode RequestPaymentConfiguration where encode = genericEncode options

-- | Constructs RequestPaymentConfiguration from required parameters
newRequestPaymentConfiguration :: Payer -> RequestPaymentConfiguration
newRequestPaymentConfiguration _Payer = RequestPaymentConfiguration { "Payer": _Payer }

-- | Constructs RequestPaymentConfiguration's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newRequestPaymentConfiguration' :: Payer -> ( { "Payer" :: (Payer) } -> {"Payer" :: (Payer) } ) -> RequestPaymentConfiguration
newRequestPaymentConfiguration' _Payer customize = (RequestPaymentConfiguration <<< customize) { "Payer": _Payer }



newtype ResponseCacheControl = ResponseCacheControl String
derive instance newtypeResponseCacheControl :: Newtype ResponseCacheControl _
derive instance repGenericResponseCacheControl :: Generic ResponseCacheControl _
instance showResponseCacheControl :: Show ResponseCacheControl where show = genericShow
instance decodeResponseCacheControl :: Decode ResponseCacheControl where decode = genericDecode options
instance encodeResponseCacheControl :: Encode ResponseCacheControl where encode = genericEncode options



newtype ResponseContentDisposition = ResponseContentDisposition String
derive instance newtypeResponseContentDisposition :: Newtype ResponseContentDisposition _
derive instance repGenericResponseContentDisposition :: Generic ResponseContentDisposition _
instance showResponseContentDisposition :: Show ResponseContentDisposition where show = genericShow
instance decodeResponseContentDisposition :: Decode ResponseContentDisposition where decode = genericDecode options
instance encodeResponseContentDisposition :: Encode ResponseContentDisposition where encode = genericEncode options



newtype ResponseContentEncoding = ResponseContentEncoding String
derive instance newtypeResponseContentEncoding :: Newtype ResponseContentEncoding _
derive instance repGenericResponseContentEncoding :: Generic ResponseContentEncoding _
instance showResponseContentEncoding :: Show ResponseContentEncoding where show = genericShow
instance decodeResponseContentEncoding :: Decode ResponseContentEncoding where decode = genericDecode options
instance encodeResponseContentEncoding :: Encode ResponseContentEncoding where encode = genericEncode options



newtype ResponseContentLanguage = ResponseContentLanguage String
derive instance newtypeResponseContentLanguage :: Newtype ResponseContentLanguage _
derive instance repGenericResponseContentLanguage :: Generic ResponseContentLanguage _
instance showResponseContentLanguage :: Show ResponseContentLanguage where show = genericShow
instance decodeResponseContentLanguage :: Decode ResponseContentLanguage where decode = genericDecode options
instance encodeResponseContentLanguage :: Encode ResponseContentLanguage where encode = genericEncode options



newtype ResponseContentType = ResponseContentType String
derive instance newtypeResponseContentType :: Newtype ResponseContentType _
derive instance repGenericResponseContentType :: Generic ResponseContentType _
instance showResponseContentType :: Show ResponseContentType where show = genericShow
instance decodeResponseContentType :: Decode ResponseContentType where decode = genericDecode options
instance encodeResponseContentType :: Encode ResponseContentType where encode = genericEncode options



newtype ResponseExpires = ResponseExpires Types.Timestamp
derive instance newtypeResponseExpires :: Newtype ResponseExpires _
derive instance repGenericResponseExpires :: Generic ResponseExpires _
instance showResponseExpires :: Show ResponseExpires where show = genericShow
instance decodeResponseExpires :: Decode ResponseExpires where decode = genericDecode options
instance encodeResponseExpires :: Encode ResponseExpires where encode = genericEncode options



newtype Restore = Restore String
derive instance newtypeRestore :: Newtype Restore _
derive instance repGenericRestore :: Generic Restore _
instance showRestore :: Show Restore where show = genericShow
instance decodeRestore :: Decode Restore where decode = genericDecode options
instance encodeRestore :: Encode Restore where encode = genericEncode options



newtype RestoreObjectOutput = RestoreObjectOutput 
  { "RequestCharged" :: NullOrUndefined (RequestCharged)
  , "RestoreOutputPath" :: NullOrUndefined (RestoreOutputPath)
  }
derive instance newtypeRestoreObjectOutput :: Newtype RestoreObjectOutput _
derive instance repGenericRestoreObjectOutput :: Generic RestoreObjectOutput _
instance showRestoreObjectOutput :: Show RestoreObjectOutput where show = genericShow
instance decodeRestoreObjectOutput :: Decode RestoreObjectOutput where decode = genericDecode options
instance encodeRestoreObjectOutput :: Encode RestoreObjectOutput where encode = genericEncode options

-- | Constructs RestoreObjectOutput from required parameters
newRestoreObjectOutput :: RestoreObjectOutput
newRestoreObjectOutput  = RestoreObjectOutput { "RequestCharged": (NullOrUndefined Nothing), "RestoreOutputPath": (NullOrUndefined Nothing) }

-- | Constructs RestoreObjectOutput's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newRestoreObjectOutput' :: ( { "RequestCharged" :: NullOrUndefined (RequestCharged) , "RestoreOutputPath" :: NullOrUndefined (RestoreOutputPath) } -> {"RequestCharged" :: NullOrUndefined (RequestCharged) , "RestoreOutputPath" :: NullOrUndefined (RestoreOutputPath) } ) -> RestoreObjectOutput
newRestoreObjectOutput'  customize = (RestoreObjectOutput <<< customize) { "RequestCharged": (NullOrUndefined Nothing), "RestoreOutputPath": (NullOrUndefined Nothing) }



newtype RestoreObjectRequest = RestoreObjectRequest 
  { "Bucket" :: (BucketName)
  , "Key" :: (ObjectKey)
  , "VersionId" :: NullOrUndefined (ObjectVersionId)
  , "RestoreRequest" :: NullOrUndefined (RestoreRequest)
  , "RequestPayer" :: NullOrUndefined (RequestPayer)
  }
derive instance newtypeRestoreObjectRequest :: Newtype RestoreObjectRequest _
derive instance repGenericRestoreObjectRequest :: Generic RestoreObjectRequest _
instance showRestoreObjectRequest :: Show RestoreObjectRequest where show = genericShow
instance decodeRestoreObjectRequest :: Decode RestoreObjectRequest where decode = genericDecode options
instance encodeRestoreObjectRequest :: Encode RestoreObjectRequest where encode = genericEncode options

-- | Constructs RestoreObjectRequest from required parameters
newRestoreObjectRequest :: BucketName -> ObjectKey -> RestoreObjectRequest
newRestoreObjectRequest _Bucket _Key = RestoreObjectRequest { "Bucket": _Bucket, "Key": _Key, "RequestPayer": (NullOrUndefined Nothing), "RestoreRequest": (NullOrUndefined Nothing), "VersionId": (NullOrUndefined Nothing) }

-- | Constructs RestoreObjectRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newRestoreObjectRequest' :: BucketName -> ObjectKey -> ( { "Bucket" :: (BucketName) , "Key" :: (ObjectKey) , "VersionId" :: NullOrUndefined (ObjectVersionId) , "RestoreRequest" :: NullOrUndefined (RestoreRequest) , "RequestPayer" :: NullOrUndefined (RequestPayer) } -> {"Bucket" :: (BucketName) , "Key" :: (ObjectKey) , "VersionId" :: NullOrUndefined (ObjectVersionId) , "RestoreRequest" :: NullOrUndefined (RestoreRequest) , "RequestPayer" :: NullOrUndefined (RequestPayer) } ) -> RestoreObjectRequest
newRestoreObjectRequest' _Bucket _Key customize = (RestoreObjectRequest <<< customize) { "Bucket": _Bucket, "Key": _Key, "RequestPayer": (NullOrUndefined Nothing), "RestoreRequest": (NullOrUndefined Nothing), "VersionId": (NullOrUndefined Nothing) }



newtype RestoreOutputPath = RestoreOutputPath String
derive instance newtypeRestoreOutputPath :: Newtype RestoreOutputPath _
derive instance repGenericRestoreOutputPath :: Generic RestoreOutputPath _
instance showRestoreOutputPath :: Show RestoreOutputPath where show = genericShow
instance decodeRestoreOutputPath :: Decode RestoreOutputPath where decode = genericDecode options
instance encodeRestoreOutputPath :: Encode RestoreOutputPath where encode = genericEncode options



-- | Container for restore job parameters.
newtype RestoreRequest = RestoreRequest 
  { "Days" :: NullOrUndefined (Days)
  , "GlacierJobParameters" :: NullOrUndefined (GlacierJobParameters)
  , "Type" :: NullOrUndefined (RestoreRequestType)
  , "Tier" :: NullOrUndefined (Tier)
  , "Description" :: NullOrUndefined (Description)
  , "SelectParameters" :: NullOrUndefined (SelectParameters)
  , "OutputLocation" :: NullOrUndefined (OutputLocation)
  }
derive instance newtypeRestoreRequest :: Newtype RestoreRequest _
derive instance repGenericRestoreRequest :: Generic RestoreRequest _
instance showRestoreRequest :: Show RestoreRequest where show = genericShow
instance decodeRestoreRequest :: Decode RestoreRequest where decode = genericDecode options
instance encodeRestoreRequest :: Encode RestoreRequest where encode = genericEncode options

-- | Constructs RestoreRequest from required parameters
newRestoreRequest :: RestoreRequest
newRestoreRequest  = RestoreRequest { "Days": (NullOrUndefined Nothing), "Description": (NullOrUndefined Nothing), "GlacierJobParameters": (NullOrUndefined Nothing), "OutputLocation": (NullOrUndefined Nothing), "SelectParameters": (NullOrUndefined Nothing), "Tier": (NullOrUndefined Nothing), "Type": (NullOrUndefined Nothing) }

-- | Constructs RestoreRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newRestoreRequest' :: ( { "Days" :: NullOrUndefined (Days) , "GlacierJobParameters" :: NullOrUndefined (GlacierJobParameters) , "Type" :: NullOrUndefined (RestoreRequestType) , "Tier" :: NullOrUndefined (Tier) , "Description" :: NullOrUndefined (Description) , "SelectParameters" :: NullOrUndefined (SelectParameters) , "OutputLocation" :: NullOrUndefined (OutputLocation) } -> {"Days" :: NullOrUndefined (Days) , "GlacierJobParameters" :: NullOrUndefined (GlacierJobParameters) , "Type" :: NullOrUndefined (RestoreRequestType) , "Tier" :: NullOrUndefined (Tier) , "Description" :: NullOrUndefined (Description) , "SelectParameters" :: NullOrUndefined (SelectParameters) , "OutputLocation" :: NullOrUndefined (OutputLocation) } ) -> RestoreRequest
newRestoreRequest'  customize = (RestoreRequest <<< customize) { "Days": (NullOrUndefined Nothing), "Description": (NullOrUndefined Nothing), "GlacierJobParameters": (NullOrUndefined Nothing), "OutputLocation": (NullOrUndefined Nothing), "SelectParameters": (NullOrUndefined Nothing), "Tier": (NullOrUndefined Nothing), "Type": (NullOrUndefined Nothing) }



newtype RestoreRequestType = RestoreRequestType String
derive instance newtypeRestoreRequestType :: Newtype RestoreRequestType _
derive instance repGenericRestoreRequestType :: Generic RestoreRequestType _
instance showRestoreRequestType :: Show RestoreRequestType where show = genericShow
instance decodeRestoreRequestType :: Decode RestoreRequestType where decode = genericDecode options
instance encodeRestoreRequestType :: Encode RestoreRequestType where encode = genericEncode options



newtype Role = Role String
derive instance newtypeRole :: Newtype Role _
derive instance repGenericRole :: Generic Role _
instance showRole :: Show Role where show = genericShow
instance decodeRole :: Decode Role where decode = genericDecode options
instance encodeRole :: Encode Role where encode = genericEncode options



newtype RoutingRule = RoutingRule 
  { "Condition" :: NullOrUndefined (Condition)
  , "Redirect" :: (Redirect)
  }
derive instance newtypeRoutingRule :: Newtype RoutingRule _
derive instance repGenericRoutingRule :: Generic RoutingRule _
instance showRoutingRule :: Show RoutingRule where show = genericShow
instance decodeRoutingRule :: Decode RoutingRule where decode = genericDecode options
instance encodeRoutingRule :: Encode RoutingRule where encode = genericEncode options

-- | Constructs RoutingRule from required parameters
newRoutingRule :: Redirect -> RoutingRule
newRoutingRule _Redirect = RoutingRule { "Redirect": _Redirect, "Condition": (NullOrUndefined Nothing) }

-- | Constructs RoutingRule's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newRoutingRule' :: Redirect -> ( { "Condition" :: NullOrUndefined (Condition) , "Redirect" :: (Redirect) } -> {"Condition" :: NullOrUndefined (Condition) , "Redirect" :: (Redirect) } ) -> RoutingRule
newRoutingRule' _Redirect customize = (RoutingRule <<< customize) { "Redirect": _Redirect, "Condition": (NullOrUndefined Nothing) }



newtype RoutingRules = RoutingRules (Array RoutingRule)
derive instance newtypeRoutingRules :: Newtype RoutingRules _
derive instance repGenericRoutingRules :: Generic RoutingRules _
instance showRoutingRules :: Show RoutingRules where show = genericShow
instance decodeRoutingRules :: Decode RoutingRules where decode = genericDecode options
instance encodeRoutingRules :: Encode RoutingRules where encode = genericEncode options



newtype Rule = Rule 
  { "Expiration" :: NullOrUndefined (LifecycleExpiration)
  , "ID" :: NullOrUndefined (ID)
  , "Prefix" :: (Prefix)
  , "Status" :: (ExpirationStatus)
  , "Transition" :: NullOrUndefined (Transition)
  , "NoncurrentVersionTransition" :: NullOrUndefined (NoncurrentVersionTransition)
  , "NoncurrentVersionExpiration" :: NullOrUndefined (NoncurrentVersionExpiration)
  , "AbortIncompleteMultipartUpload" :: NullOrUndefined (AbortIncompleteMultipartUpload)
  }
derive instance newtypeRule :: Newtype Rule _
derive instance repGenericRule :: Generic Rule _
instance showRule :: Show Rule where show = genericShow
instance decodeRule :: Decode Rule where decode = genericDecode options
instance encodeRule :: Encode Rule where encode = genericEncode options

-- | Constructs Rule from required parameters
newRule :: Prefix -> ExpirationStatus -> Rule
newRule _Prefix _Status = Rule { "Prefix": _Prefix, "Status": _Status, "AbortIncompleteMultipartUpload": (NullOrUndefined Nothing), "Expiration": (NullOrUndefined Nothing), "ID": (NullOrUndefined Nothing), "NoncurrentVersionExpiration": (NullOrUndefined Nothing), "NoncurrentVersionTransition": (NullOrUndefined Nothing), "Transition": (NullOrUndefined Nothing) }

-- | Constructs Rule's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newRule' :: Prefix -> ExpirationStatus -> ( { "Expiration" :: NullOrUndefined (LifecycleExpiration) , "ID" :: NullOrUndefined (ID) , "Prefix" :: (Prefix) , "Status" :: (ExpirationStatus) , "Transition" :: NullOrUndefined (Transition) , "NoncurrentVersionTransition" :: NullOrUndefined (NoncurrentVersionTransition) , "NoncurrentVersionExpiration" :: NullOrUndefined (NoncurrentVersionExpiration) , "AbortIncompleteMultipartUpload" :: NullOrUndefined (AbortIncompleteMultipartUpload) } -> {"Expiration" :: NullOrUndefined (LifecycleExpiration) , "ID" :: NullOrUndefined (ID) , "Prefix" :: (Prefix) , "Status" :: (ExpirationStatus) , "Transition" :: NullOrUndefined (Transition) , "NoncurrentVersionTransition" :: NullOrUndefined (NoncurrentVersionTransition) , "NoncurrentVersionExpiration" :: NullOrUndefined (NoncurrentVersionExpiration) , "AbortIncompleteMultipartUpload" :: NullOrUndefined (AbortIncompleteMultipartUpload) } ) -> Rule
newRule' _Prefix _Status customize = (Rule <<< customize) { "Prefix": _Prefix, "Status": _Status, "AbortIncompleteMultipartUpload": (NullOrUndefined Nothing), "Expiration": (NullOrUndefined Nothing), "ID": (NullOrUndefined Nothing), "NoncurrentVersionExpiration": (NullOrUndefined Nothing), "NoncurrentVersionTransition": (NullOrUndefined Nothing), "Transition": (NullOrUndefined Nothing) }



newtype Rules = Rules (Array Rule)
derive instance newtypeRules :: Newtype Rules _
derive instance repGenericRules :: Generic Rules _
instance showRules :: Show Rules where show = genericShow
instance decodeRules :: Decode Rules where decode = genericDecode options
instance encodeRules :: Encode Rules where encode = genericEncode options



-- | Container for object key name prefix and suffix filtering rules.
newtype S3KeyFilter = S3KeyFilter 
  { "FilterRules" :: NullOrUndefined (FilterRuleList)
  }
derive instance newtypeS3KeyFilter :: Newtype S3KeyFilter _
derive instance repGenericS3KeyFilter :: Generic S3KeyFilter _
instance showS3KeyFilter :: Show S3KeyFilter where show = genericShow
instance decodeS3KeyFilter :: Decode S3KeyFilter where decode = genericDecode options
instance encodeS3KeyFilter :: Encode S3KeyFilter where encode = genericEncode options

-- | Constructs S3KeyFilter from required parameters
newS3KeyFilter :: S3KeyFilter
newS3KeyFilter  = S3KeyFilter { "FilterRules": (NullOrUndefined Nothing) }

-- | Constructs S3KeyFilter's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newS3KeyFilter' :: ( { "FilterRules" :: NullOrUndefined (FilterRuleList) } -> {"FilterRules" :: NullOrUndefined (FilterRuleList) } ) -> S3KeyFilter
newS3KeyFilter'  customize = (S3KeyFilter <<< customize) { "FilterRules": (NullOrUndefined Nothing) }



-- | Describes an S3 location that will receive the results of the restore request.
newtype S3Location = S3Location 
  { "BucketName" :: (BucketName)
  , "Prefix" :: (LocationPrefix)
  , "Encryption" :: NullOrUndefined (Encryption)
  , "CannedACL" :: NullOrUndefined (ObjectCannedACL)
  , "AccessControlList" :: NullOrUndefined (Grants)
  , "Tagging" :: NullOrUndefined (Tagging)
  , "UserMetadata" :: NullOrUndefined (UserMetadata)
  , "StorageClass" :: NullOrUndefined (StorageClass)
  }
derive instance newtypeS3Location :: Newtype S3Location _
derive instance repGenericS3Location :: Generic S3Location _
instance showS3Location :: Show S3Location where show = genericShow
instance decodeS3Location :: Decode S3Location where decode = genericDecode options
instance encodeS3Location :: Encode S3Location where encode = genericEncode options

-- | Constructs S3Location from required parameters
newS3Location :: BucketName -> LocationPrefix -> S3Location
newS3Location _BucketName _Prefix = S3Location { "BucketName": _BucketName, "Prefix": _Prefix, "AccessControlList": (NullOrUndefined Nothing), "CannedACL": (NullOrUndefined Nothing), "Encryption": (NullOrUndefined Nothing), "StorageClass": (NullOrUndefined Nothing), "Tagging": (NullOrUndefined Nothing), "UserMetadata": (NullOrUndefined Nothing) }

-- | Constructs S3Location's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newS3Location' :: BucketName -> LocationPrefix -> ( { "BucketName" :: (BucketName) , "Prefix" :: (LocationPrefix) , "Encryption" :: NullOrUndefined (Encryption) , "CannedACL" :: NullOrUndefined (ObjectCannedACL) , "AccessControlList" :: NullOrUndefined (Grants) , "Tagging" :: NullOrUndefined (Tagging) , "UserMetadata" :: NullOrUndefined (UserMetadata) , "StorageClass" :: NullOrUndefined (StorageClass) } -> {"BucketName" :: (BucketName) , "Prefix" :: (LocationPrefix) , "Encryption" :: NullOrUndefined (Encryption) , "CannedACL" :: NullOrUndefined (ObjectCannedACL) , "AccessControlList" :: NullOrUndefined (Grants) , "Tagging" :: NullOrUndefined (Tagging) , "UserMetadata" :: NullOrUndefined (UserMetadata) , "StorageClass" :: NullOrUndefined (StorageClass) } ) -> S3Location
newS3Location' _BucketName _Prefix customize = (S3Location <<< customize) { "BucketName": _BucketName, "Prefix": _Prefix, "AccessControlList": (NullOrUndefined Nothing), "CannedACL": (NullOrUndefined Nothing), "Encryption": (NullOrUndefined Nothing), "StorageClass": (NullOrUndefined Nothing), "Tagging": (NullOrUndefined Nothing), "UserMetadata": (NullOrUndefined Nothing) }



newtype SSECustomerAlgorithm = SSECustomerAlgorithm String
derive instance newtypeSSECustomerAlgorithm :: Newtype SSECustomerAlgorithm _
derive instance repGenericSSECustomerAlgorithm :: Generic SSECustomerAlgorithm _
instance showSSECustomerAlgorithm :: Show SSECustomerAlgorithm where show = genericShow
instance decodeSSECustomerAlgorithm :: Decode SSECustomerAlgorithm where decode = genericDecode options
instance encodeSSECustomerAlgorithm :: Encode SSECustomerAlgorithm where encode = genericEncode options



newtype SSECustomerKey = SSECustomerKey String
derive instance newtypeSSECustomerKey :: Newtype SSECustomerKey _
derive instance repGenericSSECustomerKey :: Generic SSECustomerKey _
instance showSSECustomerKey :: Show SSECustomerKey where show = genericShow
instance decodeSSECustomerKey :: Decode SSECustomerKey where decode = genericDecode options
instance encodeSSECustomerKey :: Encode SSECustomerKey where encode = genericEncode options



newtype SSECustomerKeyMD5 = SSECustomerKeyMD5 String
derive instance newtypeSSECustomerKeyMD5 :: Newtype SSECustomerKeyMD5 _
derive instance repGenericSSECustomerKeyMD5 :: Generic SSECustomerKeyMD5 _
instance showSSECustomerKeyMD5 :: Show SSECustomerKeyMD5 where show = genericShow
instance decodeSSECustomerKeyMD5 :: Decode SSECustomerKeyMD5 where decode = genericDecode options
instance encodeSSECustomerKeyMD5 :: Encode SSECustomerKeyMD5 where encode = genericEncode options



-- | Specifies the use of SSE-KMS to encrypt delievered Inventory reports.
newtype SSEKMS = SSEKMS 
  { "KeyId" :: (SSEKMSKeyId)
  }
derive instance newtypeSSEKMS :: Newtype SSEKMS _
derive instance repGenericSSEKMS :: Generic SSEKMS _
instance showSSEKMS :: Show SSEKMS where show = genericShow
instance decodeSSEKMS :: Decode SSEKMS where decode = genericDecode options
instance encodeSSEKMS :: Encode SSEKMS where encode = genericEncode options

-- | Constructs SSEKMS from required parameters
newSSEKMS :: SSEKMSKeyId -> SSEKMS
newSSEKMS _KeyId = SSEKMS { "KeyId": _KeyId }

-- | Constructs SSEKMS's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newSSEKMS' :: SSEKMSKeyId -> ( { "KeyId" :: (SSEKMSKeyId) } -> {"KeyId" :: (SSEKMSKeyId) } ) -> SSEKMS
newSSEKMS' _KeyId customize = (SSEKMS <<< customize) { "KeyId": _KeyId }



newtype SSEKMSKeyId = SSEKMSKeyId String
derive instance newtypeSSEKMSKeyId :: Newtype SSEKMSKeyId _
derive instance repGenericSSEKMSKeyId :: Generic SSEKMSKeyId _
instance showSSEKMSKeyId :: Show SSEKMSKeyId where show = genericShow
instance decodeSSEKMSKeyId :: Decode SSEKMSKeyId where decode = genericDecode options
instance encodeSSEKMSKeyId :: Encode SSEKMSKeyId where encode = genericEncode options



-- | Specifies the use of SSE-S3 to encrypt delievered Inventory reports.
newtype SSES3 = SSES3 Types.NoArguments
derive instance newtypeSSES3 :: Newtype SSES3 _
derive instance repGenericSSES3 :: Generic SSES3 _
instance showSSES3 :: Show SSES3 where show = genericShow
instance decodeSSES3 :: Decode SSES3 where decode = genericDecode options
instance encodeSSES3 :: Encode SSES3 where encode = genericEncode options



-- | Describes the parameters for Select job types.
newtype SelectParameters = SelectParameters 
  { "InputSerialization" :: (InputSerialization)
  , "ExpressionType" :: (ExpressionType)
  , "Expression" :: (Expression)
  , "OutputSerialization" :: (OutputSerialization)
  }
derive instance newtypeSelectParameters :: Newtype SelectParameters _
derive instance repGenericSelectParameters :: Generic SelectParameters _
instance showSelectParameters :: Show SelectParameters where show = genericShow
instance decodeSelectParameters :: Decode SelectParameters where decode = genericDecode options
instance encodeSelectParameters :: Encode SelectParameters where encode = genericEncode options

-- | Constructs SelectParameters from required parameters
newSelectParameters :: Expression -> ExpressionType -> InputSerialization -> OutputSerialization -> SelectParameters
newSelectParameters _Expression _ExpressionType _InputSerialization _OutputSerialization = SelectParameters { "Expression": _Expression, "ExpressionType": _ExpressionType, "InputSerialization": _InputSerialization, "OutputSerialization": _OutputSerialization }

-- | Constructs SelectParameters's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newSelectParameters' :: Expression -> ExpressionType -> InputSerialization -> OutputSerialization -> ( { "InputSerialization" :: (InputSerialization) , "ExpressionType" :: (ExpressionType) , "Expression" :: (Expression) , "OutputSerialization" :: (OutputSerialization) } -> {"InputSerialization" :: (InputSerialization) , "ExpressionType" :: (ExpressionType) , "Expression" :: (Expression) , "OutputSerialization" :: (OutputSerialization) } ) -> SelectParameters
newSelectParameters' _Expression _ExpressionType _InputSerialization _OutputSerialization customize = (SelectParameters <<< customize) { "Expression": _Expression, "ExpressionType": _ExpressionType, "InputSerialization": _InputSerialization, "OutputSerialization": _OutputSerialization }



newtype ServerSideEncryption = ServerSideEncryption String
derive instance newtypeServerSideEncryption :: Newtype ServerSideEncryption _
derive instance repGenericServerSideEncryption :: Generic ServerSideEncryption _
instance showServerSideEncryption :: Show ServerSideEncryption where show = genericShow
instance decodeServerSideEncryption :: Decode ServerSideEncryption where decode = genericDecode options
instance encodeServerSideEncryption :: Encode ServerSideEncryption where encode = genericEncode options



-- | Describes the default server-side encryption to apply to new objects in the bucket. If Put Object request does not specify any server-side encryption, this default encryption will be applied.
newtype ServerSideEncryptionByDefault = ServerSideEncryptionByDefault 
  { "SSEAlgorithm" :: (ServerSideEncryption)
  , "KMSMasterKeyID" :: NullOrUndefined (SSEKMSKeyId)
  }
derive instance newtypeServerSideEncryptionByDefault :: Newtype ServerSideEncryptionByDefault _
derive instance repGenericServerSideEncryptionByDefault :: Generic ServerSideEncryptionByDefault _
instance showServerSideEncryptionByDefault :: Show ServerSideEncryptionByDefault where show = genericShow
instance decodeServerSideEncryptionByDefault :: Decode ServerSideEncryptionByDefault where decode = genericDecode options
instance encodeServerSideEncryptionByDefault :: Encode ServerSideEncryptionByDefault where encode = genericEncode options

-- | Constructs ServerSideEncryptionByDefault from required parameters
newServerSideEncryptionByDefault :: ServerSideEncryption -> ServerSideEncryptionByDefault
newServerSideEncryptionByDefault _SSEAlgorithm = ServerSideEncryptionByDefault { "SSEAlgorithm": _SSEAlgorithm, "KMSMasterKeyID": (NullOrUndefined Nothing) }

-- | Constructs ServerSideEncryptionByDefault's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newServerSideEncryptionByDefault' :: ServerSideEncryption -> ( { "SSEAlgorithm" :: (ServerSideEncryption) , "KMSMasterKeyID" :: NullOrUndefined (SSEKMSKeyId) } -> {"SSEAlgorithm" :: (ServerSideEncryption) , "KMSMasterKeyID" :: NullOrUndefined (SSEKMSKeyId) } ) -> ServerSideEncryptionByDefault
newServerSideEncryptionByDefault' _SSEAlgorithm customize = (ServerSideEncryptionByDefault <<< customize) { "SSEAlgorithm": _SSEAlgorithm, "KMSMasterKeyID": (NullOrUndefined Nothing) }



-- | Container for server-side encryption configuration rules. Currently S3 supports one rule only.
newtype ServerSideEncryptionConfiguration = ServerSideEncryptionConfiguration 
  { "Rules" :: (ServerSideEncryptionRules)
  }
derive instance newtypeServerSideEncryptionConfiguration :: Newtype ServerSideEncryptionConfiguration _
derive instance repGenericServerSideEncryptionConfiguration :: Generic ServerSideEncryptionConfiguration _
instance showServerSideEncryptionConfiguration :: Show ServerSideEncryptionConfiguration where show = genericShow
instance decodeServerSideEncryptionConfiguration :: Decode ServerSideEncryptionConfiguration where decode = genericDecode options
instance encodeServerSideEncryptionConfiguration :: Encode ServerSideEncryptionConfiguration where encode = genericEncode options

-- | Constructs ServerSideEncryptionConfiguration from required parameters
newServerSideEncryptionConfiguration :: ServerSideEncryptionRules -> ServerSideEncryptionConfiguration
newServerSideEncryptionConfiguration _Rules = ServerSideEncryptionConfiguration { "Rules": _Rules }

-- | Constructs ServerSideEncryptionConfiguration's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newServerSideEncryptionConfiguration' :: ServerSideEncryptionRules -> ( { "Rules" :: (ServerSideEncryptionRules) } -> {"Rules" :: (ServerSideEncryptionRules) } ) -> ServerSideEncryptionConfiguration
newServerSideEncryptionConfiguration' _Rules customize = (ServerSideEncryptionConfiguration <<< customize) { "Rules": _Rules }



-- | Container for information about a particular server-side encryption configuration rule.
newtype ServerSideEncryptionRule = ServerSideEncryptionRule 
  { "ApplyServerSideEncryptionByDefault" :: NullOrUndefined (ServerSideEncryptionByDefault)
  }
derive instance newtypeServerSideEncryptionRule :: Newtype ServerSideEncryptionRule _
derive instance repGenericServerSideEncryptionRule :: Generic ServerSideEncryptionRule _
instance showServerSideEncryptionRule :: Show ServerSideEncryptionRule where show = genericShow
instance decodeServerSideEncryptionRule :: Decode ServerSideEncryptionRule where decode = genericDecode options
instance encodeServerSideEncryptionRule :: Encode ServerSideEncryptionRule where encode = genericEncode options

-- | Constructs ServerSideEncryptionRule from required parameters
newServerSideEncryptionRule :: ServerSideEncryptionRule
newServerSideEncryptionRule  = ServerSideEncryptionRule { "ApplyServerSideEncryptionByDefault": (NullOrUndefined Nothing) }

-- | Constructs ServerSideEncryptionRule's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newServerSideEncryptionRule' :: ( { "ApplyServerSideEncryptionByDefault" :: NullOrUndefined (ServerSideEncryptionByDefault) } -> {"ApplyServerSideEncryptionByDefault" :: NullOrUndefined (ServerSideEncryptionByDefault) } ) -> ServerSideEncryptionRule
newServerSideEncryptionRule'  customize = (ServerSideEncryptionRule <<< customize) { "ApplyServerSideEncryptionByDefault": (NullOrUndefined Nothing) }



newtype ServerSideEncryptionRules = ServerSideEncryptionRules (Array ServerSideEncryptionRule)
derive instance newtypeServerSideEncryptionRules :: Newtype ServerSideEncryptionRules _
derive instance repGenericServerSideEncryptionRules :: Generic ServerSideEncryptionRules _
instance showServerSideEncryptionRules :: Show ServerSideEncryptionRules where show = genericShow
instance decodeServerSideEncryptionRules :: Decode ServerSideEncryptionRules where decode = genericDecode options
instance encodeServerSideEncryptionRules :: Encode ServerSideEncryptionRules where encode = genericEncode options



newtype Size = Size Int
derive instance newtypeSize :: Newtype Size _
derive instance repGenericSize :: Generic Size _
instance showSize :: Show Size where show = genericShow
instance decodeSize :: Decode Size where decode = genericDecode options
instance encodeSize :: Encode Size where encode = genericEncode options



-- | Container for filters that define which source objects should be replicated.
newtype SourceSelectionCriteria = SourceSelectionCriteria 
  { "SseKmsEncryptedObjects" :: NullOrUndefined (SseKmsEncryptedObjects)
  }
derive instance newtypeSourceSelectionCriteria :: Newtype SourceSelectionCriteria _
derive instance repGenericSourceSelectionCriteria :: Generic SourceSelectionCriteria _
instance showSourceSelectionCriteria :: Show SourceSelectionCriteria where show = genericShow
instance decodeSourceSelectionCriteria :: Decode SourceSelectionCriteria where decode = genericDecode options
instance encodeSourceSelectionCriteria :: Encode SourceSelectionCriteria where encode = genericEncode options

-- | Constructs SourceSelectionCriteria from required parameters
newSourceSelectionCriteria :: SourceSelectionCriteria
newSourceSelectionCriteria  = SourceSelectionCriteria { "SseKmsEncryptedObjects": (NullOrUndefined Nothing) }

-- | Constructs SourceSelectionCriteria's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newSourceSelectionCriteria' :: ( { "SseKmsEncryptedObjects" :: NullOrUndefined (SseKmsEncryptedObjects) } -> {"SseKmsEncryptedObjects" :: NullOrUndefined (SseKmsEncryptedObjects) } ) -> SourceSelectionCriteria
newSourceSelectionCriteria'  customize = (SourceSelectionCriteria <<< customize) { "SseKmsEncryptedObjects": (NullOrUndefined Nothing) }



-- | Container for filter information of selection of KMS Encrypted S3 objects.
newtype SseKmsEncryptedObjects = SseKmsEncryptedObjects 
  { "Status" :: (SseKmsEncryptedObjectsStatus)
  }
derive instance newtypeSseKmsEncryptedObjects :: Newtype SseKmsEncryptedObjects _
derive instance repGenericSseKmsEncryptedObjects :: Generic SseKmsEncryptedObjects _
instance showSseKmsEncryptedObjects :: Show SseKmsEncryptedObjects where show = genericShow
instance decodeSseKmsEncryptedObjects :: Decode SseKmsEncryptedObjects where decode = genericDecode options
instance encodeSseKmsEncryptedObjects :: Encode SseKmsEncryptedObjects where encode = genericEncode options

-- | Constructs SseKmsEncryptedObjects from required parameters
newSseKmsEncryptedObjects :: SseKmsEncryptedObjectsStatus -> SseKmsEncryptedObjects
newSseKmsEncryptedObjects _Status = SseKmsEncryptedObjects { "Status": _Status }

-- | Constructs SseKmsEncryptedObjects's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newSseKmsEncryptedObjects' :: SseKmsEncryptedObjectsStatus -> ( { "Status" :: (SseKmsEncryptedObjectsStatus) } -> {"Status" :: (SseKmsEncryptedObjectsStatus) } ) -> SseKmsEncryptedObjects
newSseKmsEncryptedObjects' _Status customize = (SseKmsEncryptedObjects <<< customize) { "Status": _Status }



newtype SseKmsEncryptedObjectsStatus = SseKmsEncryptedObjectsStatus String
derive instance newtypeSseKmsEncryptedObjectsStatus :: Newtype SseKmsEncryptedObjectsStatus _
derive instance repGenericSseKmsEncryptedObjectsStatus :: Generic SseKmsEncryptedObjectsStatus _
instance showSseKmsEncryptedObjectsStatus :: Show SseKmsEncryptedObjectsStatus where show = genericShow
instance decodeSseKmsEncryptedObjectsStatus :: Decode SseKmsEncryptedObjectsStatus where decode = genericDecode options
instance encodeSseKmsEncryptedObjectsStatus :: Encode SseKmsEncryptedObjectsStatus where encode = genericEncode options



newtype StartAfter = StartAfter String
derive instance newtypeStartAfter :: Newtype StartAfter _
derive instance repGenericStartAfter :: Generic StartAfter _
instance showStartAfter :: Show StartAfter where show = genericShow
instance decodeStartAfter :: Decode StartAfter where decode = genericDecode options
instance encodeStartAfter :: Encode StartAfter where encode = genericEncode options



newtype StorageClass = StorageClass String
derive instance newtypeStorageClass :: Newtype StorageClass _
derive instance repGenericStorageClass :: Generic StorageClass _
instance showStorageClass :: Show StorageClass where show = genericShow
instance decodeStorageClass :: Decode StorageClass where decode = genericDecode options
instance encodeStorageClass :: Encode StorageClass where encode = genericEncode options



newtype StorageClassAnalysis = StorageClassAnalysis 
  { "DataExport" :: NullOrUndefined (StorageClassAnalysisDataExport)
  }
derive instance newtypeStorageClassAnalysis :: Newtype StorageClassAnalysis _
derive instance repGenericStorageClassAnalysis :: Generic StorageClassAnalysis _
instance showStorageClassAnalysis :: Show StorageClassAnalysis where show = genericShow
instance decodeStorageClassAnalysis :: Decode StorageClassAnalysis where decode = genericDecode options
instance encodeStorageClassAnalysis :: Encode StorageClassAnalysis where encode = genericEncode options

-- | Constructs StorageClassAnalysis from required parameters
newStorageClassAnalysis :: StorageClassAnalysis
newStorageClassAnalysis  = StorageClassAnalysis { "DataExport": (NullOrUndefined Nothing) }

-- | Constructs StorageClassAnalysis's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newStorageClassAnalysis' :: ( { "DataExport" :: NullOrUndefined (StorageClassAnalysisDataExport) } -> {"DataExport" :: NullOrUndefined (StorageClassAnalysisDataExport) } ) -> StorageClassAnalysis
newStorageClassAnalysis'  customize = (StorageClassAnalysis <<< customize) { "DataExport": (NullOrUndefined Nothing) }



newtype StorageClassAnalysisDataExport = StorageClassAnalysisDataExport 
  { "OutputSchemaVersion" :: (StorageClassAnalysisSchemaVersion)
  , "Destination" :: (AnalyticsExportDestination)
  }
derive instance newtypeStorageClassAnalysisDataExport :: Newtype StorageClassAnalysisDataExport _
derive instance repGenericStorageClassAnalysisDataExport :: Generic StorageClassAnalysisDataExport _
instance showStorageClassAnalysisDataExport :: Show StorageClassAnalysisDataExport where show = genericShow
instance decodeStorageClassAnalysisDataExport :: Decode StorageClassAnalysisDataExport where decode = genericDecode options
instance encodeStorageClassAnalysisDataExport :: Encode StorageClassAnalysisDataExport where encode = genericEncode options

-- | Constructs StorageClassAnalysisDataExport from required parameters
newStorageClassAnalysisDataExport :: AnalyticsExportDestination -> StorageClassAnalysisSchemaVersion -> StorageClassAnalysisDataExport
newStorageClassAnalysisDataExport _Destination _OutputSchemaVersion = StorageClassAnalysisDataExport { "Destination": _Destination, "OutputSchemaVersion": _OutputSchemaVersion }

-- | Constructs StorageClassAnalysisDataExport's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newStorageClassAnalysisDataExport' :: AnalyticsExportDestination -> StorageClassAnalysisSchemaVersion -> ( { "OutputSchemaVersion" :: (StorageClassAnalysisSchemaVersion) , "Destination" :: (AnalyticsExportDestination) } -> {"OutputSchemaVersion" :: (StorageClassAnalysisSchemaVersion) , "Destination" :: (AnalyticsExportDestination) } ) -> StorageClassAnalysisDataExport
newStorageClassAnalysisDataExport' _Destination _OutputSchemaVersion customize = (StorageClassAnalysisDataExport <<< customize) { "Destination": _Destination, "OutputSchemaVersion": _OutputSchemaVersion }



newtype StorageClassAnalysisSchemaVersion = StorageClassAnalysisSchemaVersion String
derive instance newtypeStorageClassAnalysisSchemaVersion :: Newtype StorageClassAnalysisSchemaVersion _
derive instance repGenericStorageClassAnalysisSchemaVersion :: Generic StorageClassAnalysisSchemaVersion _
instance showStorageClassAnalysisSchemaVersion :: Show StorageClassAnalysisSchemaVersion where show = genericShow
instance decodeStorageClassAnalysisSchemaVersion :: Decode StorageClassAnalysisSchemaVersion where decode = genericDecode options
instance encodeStorageClassAnalysisSchemaVersion :: Encode StorageClassAnalysisSchemaVersion where encode = genericEncode options



newtype Suffix = Suffix String
derive instance newtypeSuffix :: Newtype Suffix _
derive instance repGenericSuffix :: Generic Suffix _
instance showSuffix :: Show Suffix where show = genericShow
instance decodeSuffix :: Decode Suffix where decode = genericDecode options
instance encodeSuffix :: Encode Suffix where encode = genericEncode options



newtype Tag = Tag 
  { "Key" :: (ObjectKey)
  , "Value" :: (Value)
  }
derive instance newtypeTag :: Newtype Tag _
derive instance repGenericTag :: Generic Tag _
instance showTag :: Show Tag where show = genericShow
instance decodeTag :: Decode Tag where decode = genericDecode options
instance encodeTag :: Encode Tag where encode = genericEncode options

-- | Constructs Tag from required parameters
newTag :: ObjectKey -> Value -> Tag
newTag _Key _Value = Tag { "Key": _Key, "Value": _Value }

-- | Constructs Tag's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newTag' :: ObjectKey -> Value -> ( { "Key" :: (ObjectKey) , "Value" :: (Value) } -> {"Key" :: (ObjectKey) , "Value" :: (Value) } ) -> Tag
newTag' _Key _Value customize = (Tag <<< customize) { "Key": _Key, "Value": _Value }



newtype TagCount = TagCount Int
derive instance newtypeTagCount :: Newtype TagCount _
derive instance repGenericTagCount :: Generic TagCount _
instance showTagCount :: Show TagCount where show = genericShow
instance decodeTagCount :: Decode TagCount where decode = genericDecode options
instance encodeTagCount :: Encode TagCount where encode = genericEncode options



newtype TagSet = TagSet (Array Tag)
derive instance newtypeTagSet :: Newtype TagSet _
derive instance repGenericTagSet :: Generic TagSet _
instance showTagSet :: Show TagSet where show = genericShow
instance decodeTagSet :: Decode TagSet where decode = genericDecode options
instance encodeTagSet :: Encode TagSet where encode = genericEncode options



newtype Tagging = Tagging 
  { "TagSet" :: (TagSet)
  }
derive instance newtypeTagging :: Newtype Tagging _
derive instance repGenericTagging :: Generic Tagging _
instance showTagging :: Show Tagging where show = genericShow
instance decodeTagging :: Decode Tagging where decode = genericDecode options
instance encodeTagging :: Encode Tagging where encode = genericEncode options

-- | Constructs Tagging from required parameters
newTagging :: TagSet -> Tagging
newTagging _TagSet = Tagging { "TagSet": _TagSet }

-- | Constructs Tagging's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newTagging' :: TagSet -> ( { "TagSet" :: (TagSet) } -> {"TagSet" :: (TagSet) } ) -> Tagging
newTagging' _TagSet customize = (Tagging <<< customize) { "TagSet": _TagSet }



newtype TaggingDirective = TaggingDirective String
derive instance newtypeTaggingDirective :: Newtype TaggingDirective _
derive instance repGenericTaggingDirective :: Generic TaggingDirective _
instance showTaggingDirective :: Show TaggingDirective where show = genericShow
instance decodeTaggingDirective :: Decode TaggingDirective where decode = genericDecode options
instance encodeTaggingDirective :: Encode TaggingDirective where encode = genericEncode options



newtype TaggingHeader = TaggingHeader String
derive instance newtypeTaggingHeader :: Newtype TaggingHeader _
derive instance repGenericTaggingHeader :: Generic TaggingHeader _
instance showTaggingHeader :: Show TaggingHeader where show = genericShow
instance decodeTaggingHeader :: Decode TaggingHeader where decode = genericDecode options
instance encodeTaggingHeader :: Encode TaggingHeader where encode = genericEncode options



newtype TargetBucket = TargetBucket String
derive instance newtypeTargetBucket :: Newtype TargetBucket _
derive instance repGenericTargetBucket :: Generic TargetBucket _
instance showTargetBucket :: Show TargetBucket where show = genericShow
instance decodeTargetBucket :: Decode TargetBucket where decode = genericDecode options
instance encodeTargetBucket :: Encode TargetBucket where encode = genericEncode options



newtype TargetGrant = TargetGrant 
  { "Grantee" :: NullOrUndefined (Grantee)
  , "Permission" :: NullOrUndefined (BucketLogsPermission)
  }
derive instance newtypeTargetGrant :: Newtype TargetGrant _
derive instance repGenericTargetGrant :: Generic TargetGrant _
instance showTargetGrant :: Show TargetGrant where show = genericShow
instance decodeTargetGrant :: Decode TargetGrant where decode = genericDecode options
instance encodeTargetGrant :: Encode TargetGrant where encode = genericEncode options

-- | Constructs TargetGrant from required parameters
newTargetGrant :: TargetGrant
newTargetGrant  = TargetGrant { "Grantee": (NullOrUndefined Nothing), "Permission": (NullOrUndefined Nothing) }

-- | Constructs TargetGrant's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newTargetGrant' :: ( { "Grantee" :: NullOrUndefined (Grantee) , "Permission" :: NullOrUndefined (BucketLogsPermission) } -> {"Grantee" :: NullOrUndefined (Grantee) , "Permission" :: NullOrUndefined (BucketLogsPermission) } ) -> TargetGrant
newTargetGrant'  customize = (TargetGrant <<< customize) { "Grantee": (NullOrUndefined Nothing), "Permission": (NullOrUndefined Nothing) }



newtype TargetGrants = TargetGrants (Array TargetGrant)
derive instance newtypeTargetGrants :: Newtype TargetGrants _
derive instance repGenericTargetGrants :: Generic TargetGrants _
instance showTargetGrants :: Show TargetGrants where show = genericShow
instance decodeTargetGrants :: Decode TargetGrants where decode = genericDecode options
instance encodeTargetGrants :: Encode TargetGrants where encode = genericEncode options



newtype TargetPrefix = TargetPrefix String
derive instance newtypeTargetPrefix :: Newtype TargetPrefix _
derive instance repGenericTargetPrefix :: Generic TargetPrefix _
instance showTargetPrefix :: Show TargetPrefix where show = genericShow
instance decodeTargetPrefix :: Decode TargetPrefix where decode = genericDecode options
instance encodeTargetPrefix :: Encode TargetPrefix where encode = genericEncode options



newtype Tier = Tier String
derive instance newtypeTier :: Newtype Tier _
derive instance repGenericTier :: Generic Tier _
instance showTier :: Show Tier where show = genericShow
instance decodeTier :: Decode Tier where decode = genericDecode options
instance encodeTier :: Encode Tier where encode = genericEncode options



newtype Token = Token String
derive instance newtypeToken :: Newtype Token _
derive instance repGenericToken :: Generic Token _
instance showToken :: Show Token where show = genericShow
instance decodeToken :: Decode Token where decode = genericDecode options
instance encodeToken :: Encode Token where encode = genericEncode options



newtype TopicArn = TopicArn String
derive instance newtypeTopicArn :: Newtype TopicArn _
derive instance repGenericTopicArn :: Generic TopicArn _
instance showTopicArn :: Show TopicArn where show = genericShow
instance decodeTopicArn :: Decode TopicArn where decode = genericDecode options
instance encodeTopicArn :: Encode TopicArn where encode = genericEncode options



-- | Container for specifying the configuration when you want Amazon S3 to publish events to an Amazon Simple Notification Service (Amazon SNS) topic.
newtype TopicConfiguration = TopicConfiguration 
  { "Id" :: NullOrUndefined (NotificationId)
  , "TopicArn" :: (TopicArn)
  , "Events" :: (EventList)
  , "Filter" :: NullOrUndefined (NotificationConfigurationFilter)
  }
derive instance newtypeTopicConfiguration :: Newtype TopicConfiguration _
derive instance repGenericTopicConfiguration :: Generic TopicConfiguration _
instance showTopicConfiguration :: Show TopicConfiguration where show = genericShow
instance decodeTopicConfiguration :: Decode TopicConfiguration where decode = genericDecode options
instance encodeTopicConfiguration :: Encode TopicConfiguration where encode = genericEncode options

-- | Constructs TopicConfiguration from required parameters
newTopicConfiguration :: EventList -> TopicArn -> TopicConfiguration
newTopicConfiguration _Events _TopicArn = TopicConfiguration { "Events": _Events, "TopicArn": _TopicArn, "Filter": (NullOrUndefined Nothing), "Id": (NullOrUndefined Nothing) }

-- | Constructs TopicConfiguration's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newTopicConfiguration' :: EventList -> TopicArn -> ( { "Id" :: NullOrUndefined (NotificationId) , "TopicArn" :: (TopicArn) , "Events" :: (EventList) , "Filter" :: NullOrUndefined (NotificationConfigurationFilter) } -> {"Id" :: NullOrUndefined (NotificationId) , "TopicArn" :: (TopicArn) , "Events" :: (EventList) , "Filter" :: NullOrUndefined (NotificationConfigurationFilter) } ) -> TopicConfiguration
newTopicConfiguration' _Events _TopicArn customize = (TopicConfiguration <<< customize) { "Events": _Events, "TopicArn": _TopicArn, "Filter": (NullOrUndefined Nothing), "Id": (NullOrUndefined Nothing) }



newtype TopicConfigurationDeprecated = TopicConfigurationDeprecated 
  { "Id" :: NullOrUndefined (NotificationId)
  , "Events" :: NullOrUndefined (EventList)
  , "Event" :: NullOrUndefined (Event)
  , "Topic" :: NullOrUndefined (TopicArn)
  }
derive instance newtypeTopicConfigurationDeprecated :: Newtype TopicConfigurationDeprecated _
derive instance repGenericTopicConfigurationDeprecated :: Generic TopicConfigurationDeprecated _
instance showTopicConfigurationDeprecated :: Show TopicConfigurationDeprecated where show = genericShow
instance decodeTopicConfigurationDeprecated :: Decode TopicConfigurationDeprecated where decode = genericDecode options
instance encodeTopicConfigurationDeprecated :: Encode TopicConfigurationDeprecated where encode = genericEncode options

-- | Constructs TopicConfigurationDeprecated from required parameters
newTopicConfigurationDeprecated :: TopicConfigurationDeprecated
newTopicConfigurationDeprecated  = TopicConfigurationDeprecated { "Event": (NullOrUndefined Nothing), "Events": (NullOrUndefined Nothing), "Id": (NullOrUndefined Nothing), "Topic": (NullOrUndefined Nothing) }

-- | Constructs TopicConfigurationDeprecated's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newTopicConfigurationDeprecated' :: ( { "Id" :: NullOrUndefined (NotificationId) , "Events" :: NullOrUndefined (EventList) , "Event" :: NullOrUndefined (Event) , "Topic" :: NullOrUndefined (TopicArn) } -> {"Id" :: NullOrUndefined (NotificationId) , "Events" :: NullOrUndefined (EventList) , "Event" :: NullOrUndefined (Event) , "Topic" :: NullOrUndefined (TopicArn) } ) -> TopicConfigurationDeprecated
newTopicConfigurationDeprecated'  customize = (TopicConfigurationDeprecated <<< customize) { "Event": (NullOrUndefined Nothing), "Events": (NullOrUndefined Nothing), "Id": (NullOrUndefined Nothing), "Topic": (NullOrUndefined Nothing) }



newtype TopicConfigurationList = TopicConfigurationList (Array TopicConfiguration)
derive instance newtypeTopicConfigurationList :: Newtype TopicConfigurationList _
derive instance repGenericTopicConfigurationList :: Generic TopicConfigurationList _
instance showTopicConfigurationList :: Show TopicConfigurationList where show = genericShow
instance decodeTopicConfigurationList :: Decode TopicConfigurationList where decode = genericDecode options
instance encodeTopicConfigurationList :: Encode TopicConfigurationList where encode = genericEncode options



newtype Transition = Transition 
  { "Date" :: NullOrUndefined (Date)
  , "Days" :: NullOrUndefined (Days)
  , "StorageClass" :: NullOrUndefined (TransitionStorageClass)
  }
derive instance newtypeTransition :: Newtype Transition _
derive instance repGenericTransition :: Generic Transition _
instance showTransition :: Show Transition where show = genericShow
instance decodeTransition :: Decode Transition where decode = genericDecode options
instance encodeTransition :: Encode Transition where encode = genericEncode options

-- | Constructs Transition from required parameters
newTransition :: Transition
newTransition  = Transition { "Date": (NullOrUndefined Nothing), "Days": (NullOrUndefined Nothing), "StorageClass": (NullOrUndefined Nothing) }

-- | Constructs Transition's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newTransition' :: ( { "Date" :: NullOrUndefined (Date) , "Days" :: NullOrUndefined (Days) , "StorageClass" :: NullOrUndefined (TransitionStorageClass) } -> {"Date" :: NullOrUndefined (Date) , "Days" :: NullOrUndefined (Days) , "StorageClass" :: NullOrUndefined (TransitionStorageClass) } ) -> Transition
newTransition'  customize = (Transition <<< customize) { "Date": (NullOrUndefined Nothing), "Days": (NullOrUndefined Nothing), "StorageClass": (NullOrUndefined Nothing) }



newtype TransitionList = TransitionList (Array Transition)
derive instance newtypeTransitionList :: Newtype TransitionList _
derive instance repGenericTransitionList :: Generic TransitionList _
instance showTransitionList :: Show TransitionList where show = genericShow
instance decodeTransitionList :: Decode TransitionList where decode = genericDecode options
instance encodeTransitionList :: Encode TransitionList where encode = genericEncode options



newtype TransitionStorageClass = TransitionStorageClass String
derive instance newtypeTransitionStorageClass :: Newtype TransitionStorageClass _
derive instance repGenericTransitionStorageClass :: Generic TransitionStorageClass _
instance showTransitionStorageClass :: Show TransitionStorageClass where show = genericShow
instance decodeTransitionStorageClass :: Decode TransitionStorageClass where decode = genericDecode options
instance encodeTransitionStorageClass :: Encode TransitionStorageClass where encode = genericEncode options



newtype Type = Type String
derive instance newtypeType :: Newtype Type _
derive instance repGenericType :: Generic Type _
instance showType :: Show Type where show = genericShow
instance decodeType :: Decode Type where decode = genericDecode options
instance encodeType :: Encode Type where encode = genericEncode options



newtype URI = URI String
derive instance newtypeURI :: Newtype URI _
derive instance repGenericURI :: Generic URI _
instance showURI :: Show URI where show = genericShow
instance decodeURI :: Decode URI where decode = genericDecode options
instance encodeURI :: Encode URI where encode = genericEncode options



newtype UploadIdMarker = UploadIdMarker String
derive instance newtypeUploadIdMarker :: Newtype UploadIdMarker _
derive instance repGenericUploadIdMarker :: Generic UploadIdMarker _
instance showUploadIdMarker :: Show UploadIdMarker where show = genericShow
instance decodeUploadIdMarker :: Decode UploadIdMarker where decode = genericDecode options
instance encodeUploadIdMarker :: Encode UploadIdMarker where encode = genericEncode options



newtype UploadPartCopyOutput = UploadPartCopyOutput 
  { "CopySourceVersionId" :: NullOrUndefined (CopySourceVersionId)
  , "CopyPartResult" :: NullOrUndefined (CopyPartResult)
  , "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption)
  , "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm)
  , "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5)
  , "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId)
  , "RequestCharged" :: NullOrUndefined (RequestCharged)
  }
derive instance newtypeUploadPartCopyOutput :: Newtype UploadPartCopyOutput _
derive instance repGenericUploadPartCopyOutput :: Generic UploadPartCopyOutput _
instance showUploadPartCopyOutput :: Show UploadPartCopyOutput where show = genericShow
instance decodeUploadPartCopyOutput :: Decode UploadPartCopyOutput where decode = genericDecode options
instance encodeUploadPartCopyOutput :: Encode UploadPartCopyOutput where encode = genericEncode options

-- | Constructs UploadPartCopyOutput from required parameters
newUploadPartCopyOutput :: UploadPartCopyOutput
newUploadPartCopyOutput  = UploadPartCopyOutput { "CopyPartResult": (NullOrUndefined Nothing), "CopySourceVersionId": (NullOrUndefined Nothing), "RequestCharged": (NullOrUndefined Nothing), "SSECustomerAlgorithm": (NullOrUndefined Nothing), "SSECustomerKeyMD5": (NullOrUndefined Nothing), "SSEKMSKeyId": (NullOrUndefined Nothing), "ServerSideEncryption": (NullOrUndefined Nothing) }

-- | Constructs UploadPartCopyOutput's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newUploadPartCopyOutput' :: ( { "CopySourceVersionId" :: NullOrUndefined (CopySourceVersionId) , "CopyPartResult" :: NullOrUndefined (CopyPartResult) , "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption) , "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm) , "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5) , "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId) , "RequestCharged" :: NullOrUndefined (RequestCharged) } -> {"CopySourceVersionId" :: NullOrUndefined (CopySourceVersionId) , "CopyPartResult" :: NullOrUndefined (CopyPartResult) , "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption) , "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm) , "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5) , "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId) , "RequestCharged" :: NullOrUndefined (RequestCharged) } ) -> UploadPartCopyOutput
newUploadPartCopyOutput'  customize = (UploadPartCopyOutput <<< customize) { "CopyPartResult": (NullOrUndefined Nothing), "CopySourceVersionId": (NullOrUndefined Nothing), "RequestCharged": (NullOrUndefined Nothing), "SSECustomerAlgorithm": (NullOrUndefined Nothing), "SSECustomerKeyMD5": (NullOrUndefined Nothing), "SSEKMSKeyId": (NullOrUndefined Nothing), "ServerSideEncryption": (NullOrUndefined Nothing) }



newtype UploadPartCopyRequest = UploadPartCopyRequest 
  { "Bucket" :: (BucketName)
  , "CopySource" :: (CopySource)
  , "CopySourceIfMatch" :: NullOrUndefined (CopySourceIfMatch)
  , "CopySourceIfModifiedSince" :: NullOrUndefined (CopySourceIfModifiedSince)
  , "CopySourceIfNoneMatch" :: NullOrUndefined (CopySourceIfNoneMatch)
  , "CopySourceIfUnmodifiedSince" :: NullOrUndefined (CopySourceIfUnmodifiedSince)
  , "CopySourceRange" :: NullOrUndefined (CopySourceRange)
  , "Key" :: (ObjectKey)
  , "PartNumber" :: (PartNumber)
  , "UploadId" :: (MultipartUploadId)
  , "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm)
  , "SSECustomerKey" :: NullOrUndefined (SSECustomerKey)
  , "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5)
  , "CopySourceSSECustomerAlgorithm" :: NullOrUndefined (CopySourceSSECustomerAlgorithm)
  , "CopySourceSSECustomerKey" :: NullOrUndefined (CopySourceSSECustomerKey)
  , "CopySourceSSECustomerKeyMD5" :: NullOrUndefined (CopySourceSSECustomerKeyMD5)
  , "RequestPayer" :: NullOrUndefined (RequestPayer)
  }
derive instance newtypeUploadPartCopyRequest :: Newtype UploadPartCopyRequest _
derive instance repGenericUploadPartCopyRequest :: Generic UploadPartCopyRequest _
instance showUploadPartCopyRequest :: Show UploadPartCopyRequest where show = genericShow
instance decodeUploadPartCopyRequest :: Decode UploadPartCopyRequest where decode = genericDecode options
instance encodeUploadPartCopyRequest :: Encode UploadPartCopyRequest where encode = genericEncode options

-- | Constructs UploadPartCopyRequest from required parameters
newUploadPartCopyRequest :: BucketName -> CopySource -> ObjectKey -> PartNumber -> MultipartUploadId -> UploadPartCopyRequest
newUploadPartCopyRequest _Bucket _CopySource _Key _PartNumber _UploadId = UploadPartCopyRequest { "Bucket": _Bucket, "CopySource": _CopySource, "Key": _Key, "PartNumber": _PartNumber, "UploadId": _UploadId, "CopySourceIfMatch": (NullOrUndefined Nothing), "CopySourceIfModifiedSince": (NullOrUndefined Nothing), "CopySourceIfNoneMatch": (NullOrUndefined Nothing), "CopySourceIfUnmodifiedSince": (NullOrUndefined Nothing), "CopySourceRange": (NullOrUndefined Nothing), "CopySourceSSECustomerAlgorithm": (NullOrUndefined Nothing), "CopySourceSSECustomerKey": (NullOrUndefined Nothing), "CopySourceSSECustomerKeyMD5": (NullOrUndefined Nothing), "RequestPayer": (NullOrUndefined Nothing), "SSECustomerAlgorithm": (NullOrUndefined Nothing), "SSECustomerKey": (NullOrUndefined Nothing), "SSECustomerKeyMD5": (NullOrUndefined Nothing) }

-- | Constructs UploadPartCopyRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newUploadPartCopyRequest' :: BucketName -> CopySource -> ObjectKey -> PartNumber -> MultipartUploadId -> ( { "Bucket" :: (BucketName) , "CopySource" :: (CopySource) , "CopySourceIfMatch" :: NullOrUndefined (CopySourceIfMatch) , "CopySourceIfModifiedSince" :: NullOrUndefined (CopySourceIfModifiedSince) , "CopySourceIfNoneMatch" :: NullOrUndefined (CopySourceIfNoneMatch) , "CopySourceIfUnmodifiedSince" :: NullOrUndefined (CopySourceIfUnmodifiedSince) , "CopySourceRange" :: NullOrUndefined (CopySourceRange) , "Key" :: (ObjectKey) , "PartNumber" :: (PartNumber) , "UploadId" :: (MultipartUploadId) , "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm) , "SSECustomerKey" :: NullOrUndefined (SSECustomerKey) , "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5) , "CopySourceSSECustomerAlgorithm" :: NullOrUndefined (CopySourceSSECustomerAlgorithm) , "CopySourceSSECustomerKey" :: NullOrUndefined (CopySourceSSECustomerKey) , "CopySourceSSECustomerKeyMD5" :: NullOrUndefined (CopySourceSSECustomerKeyMD5) , "RequestPayer" :: NullOrUndefined (RequestPayer) } -> {"Bucket" :: (BucketName) , "CopySource" :: (CopySource) , "CopySourceIfMatch" :: NullOrUndefined (CopySourceIfMatch) , "CopySourceIfModifiedSince" :: NullOrUndefined (CopySourceIfModifiedSince) , "CopySourceIfNoneMatch" :: NullOrUndefined (CopySourceIfNoneMatch) , "CopySourceIfUnmodifiedSince" :: NullOrUndefined (CopySourceIfUnmodifiedSince) , "CopySourceRange" :: NullOrUndefined (CopySourceRange) , "Key" :: (ObjectKey) , "PartNumber" :: (PartNumber) , "UploadId" :: (MultipartUploadId) , "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm) , "SSECustomerKey" :: NullOrUndefined (SSECustomerKey) , "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5) , "CopySourceSSECustomerAlgorithm" :: NullOrUndefined (CopySourceSSECustomerAlgorithm) , "CopySourceSSECustomerKey" :: NullOrUndefined (CopySourceSSECustomerKey) , "CopySourceSSECustomerKeyMD5" :: NullOrUndefined (CopySourceSSECustomerKeyMD5) , "RequestPayer" :: NullOrUndefined (RequestPayer) } ) -> UploadPartCopyRequest
newUploadPartCopyRequest' _Bucket _CopySource _Key _PartNumber _UploadId customize = (UploadPartCopyRequest <<< customize) { "Bucket": _Bucket, "CopySource": _CopySource, "Key": _Key, "PartNumber": _PartNumber, "UploadId": _UploadId, "CopySourceIfMatch": (NullOrUndefined Nothing), "CopySourceIfModifiedSince": (NullOrUndefined Nothing), "CopySourceIfNoneMatch": (NullOrUndefined Nothing), "CopySourceIfUnmodifiedSince": (NullOrUndefined Nothing), "CopySourceRange": (NullOrUndefined Nothing), "CopySourceSSECustomerAlgorithm": (NullOrUndefined Nothing), "CopySourceSSECustomerKey": (NullOrUndefined Nothing), "CopySourceSSECustomerKeyMD5": (NullOrUndefined Nothing), "RequestPayer": (NullOrUndefined Nothing), "SSECustomerAlgorithm": (NullOrUndefined Nothing), "SSECustomerKey": (NullOrUndefined Nothing), "SSECustomerKeyMD5": (NullOrUndefined Nothing) }



newtype UploadPartOutput = UploadPartOutput 
  { "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption)
  , "ETag" :: NullOrUndefined (ETag)
  , "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm)
  , "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5)
  , "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId)
  , "RequestCharged" :: NullOrUndefined (RequestCharged)
  }
derive instance newtypeUploadPartOutput :: Newtype UploadPartOutput _
derive instance repGenericUploadPartOutput :: Generic UploadPartOutput _
instance showUploadPartOutput :: Show UploadPartOutput where show = genericShow
instance decodeUploadPartOutput :: Decode UploadPartOutput where decode = genericDecode options
instance encodeUploadPartOutput :: Encode UploadPartOutput where encode = genericEncode options

-- | Constructs UploadPartOutput from required parameters
newUploadPartOutput :: UploadPartOutput
newUploadPartOutput  = UploadPartOutput { "ETag": (NullOrUndefined Nothing), "RequestCharged": (NullOrUndefined Nothing), "SSECustomerAlgorithm": (NullOrUndefined Nothing), "SSECustomerKeyMD5": (NullOrUndefined Nothing), "SSEKMSKeyId": (NullOrUndefined Nothing), "ServerSideEncryption": (NullOrUndefined Nothing) }

-- | Constructs UploadPartOutput's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newUploadPartOutput' :: ( { "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption) , "ETag" :: NullOrUndefined (ETag) , "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm) , "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5) , "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId) , "RequestCharged" :: NullOrUndefined (RequestCharged) } -> {"ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption) , "ETag" :: NullOrUndefined (ETag) , "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm) , "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5) , "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId) , "RequestCharged" :: NullOrUndefined (RequestCharged) } ) -> UploadPartOutput
newUploadPartOutput'  customize = (UploadPartOutput <<< customize) { "ETag": (NullOrUndefined Nothing), "RequestCharged": (NullOrUndefined Nothing), "SSECustomerAlgorithm": (NullOrUndefined Nothing), "SSECustomerKeyMD5": (NullOrUndefined Nothing), "SSEKMSKeyId": (NullOrUndefined Nothing), "ServerSideEncryption": (NullOrUndefined Nothing) }



newtype UploadPartRequest = UploadPartRequest 
  { "Body" :: NullOrUndefined (Body)
  , "Bucket" :: (BucketName)
  , "ContentLength" :: NullOrUndefined (ContentLength)
  , "ContentMD5" :: NullOrUndefined (ContentMD5)
  , "Key" :: (ObjectKey)
  , "PartNumber" :: (PartNumber)
  , "UploadId" :: (MultipartUploadId)
  , "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm)
  , "SSECustomerKey" :: NullOrUndefined (SSECustomerKey)
  , "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5)
  , "RequestPayer" :: NullOrUndefined (RequestPayer)
  }
derive instance newtypeUploadPartRequest :: Newtype UploadPartRequest _
derive instance repGenericUploadPartRequest :: Generic UploadPartRequest _
instance showUploadPartRequest :: Show UploadPartRequest where show = genericShow
instance decodeUploadPartRequest :: Decode UploadPartRequest where decode = genericDecode options
instance encodeUploadPartRequest :: Encode UploadPartRequest where encode = genericEncode options

-- | Constructs UploadPartRequest from required parameters
newUploadPartRequest :: BucketName -> ObjectKey -> PartNumber -> MultipartUploadId -> UploadPartRequest
newUploadPartRequest _Bucket _Key _PartNumber _UploadId = UploadPartRequest { "Bucket": _Bucket, "Key": _Key, "PartNumber": _PartNumber, "UploadId": _UploadId, "Body": (NullOrUndefined Nothing), "ContentLength": (NullOrUndefined Nothing), "ContentMD5": (NullOrUndefined Nothing), "RequestPayer": (NullOrUndefined Nothing), "SSECustomerAlgorithm": (NullOrUndefined Nothing), "SSECustomerKey": (NullOrUndefined Nothing), "SSECustomerKeyMD5": (NullOrUndefined Nothing) }

-- | Constructs UploadPartRequest's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newUploadPartRequest' :: BucketName -> ObjectKey -> PartNumber -> MultipartUploadId -> ( { "Body" :: NullOrUndefined (Body) , "Bucket" :: (BucketName) , "ContentLength" :: NullOrUndefined (ContentLength) , "ContentMD5" :: NullOrUndefined (ContentMD5) , "Key" :: (ObjectKey) , "PartNumber" :: (PartNumber) , "UploadId" :: (MultipartUploadId) , "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm) , "SSECustomerKey" :: NullOrUndefined (SSECustomerKey) , "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5) , "RequestPayer" :: NullOrUndefined (RequestPayer) } -> {"Body" :: NullOrUndefined (Body) , "Bucket" :: (BucketName) , "ContentLength" :: NullOrUndefined (ContentLength) , "ContentMD5" :: NullOrUndefined (ContentMD5) , "Key" :: (ObjectKey) , "PartNumber" :: (PartNumber) , "UploadId" :: (MultipartUploadId) , "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm) , "SSECustomerKey" :: NullOrUndefined (SSECustomerKey) , "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5) , "RequestPayer" :: NullOrUndefined (RequestPayer) } ) -> UploadPartRequest
newUploadPartRequest' _Bucket _Key _PartNumber _UploadId customize = (UploadPartRequest <<< customize) { "Bucket": _Bucket, "Key": _Key, "PartNumber": _PartNumber, "UploadId": _UploadId, "Body": (NullOrUndefined Nothing), "ContentLength": (NullOrUndefined Nothing), "ContentMD5": (NullOrUndefined Nothing), "RequestPayer": (NullOrUndefined Nothing), "SSECustomerAlgorithm": (NullOrUndefined Nothing), "SSECustomerKey": (NullOrUndefined Nothing), "SSECustomerKeyMD5": (NullOrUndefined Nothing) }



newtype UserMetadata = UserMetadata (Array MetadataEntry)
derive instance newtypeUserMetadata :: Newtype UserMetadata _
derive instance repGenericUserMetadata :: Generic UserMetadata _
instance showUserMetadata :: Show UserMetadata where show = genericShow
instance decodeUserMetadata :: Decode UserMetadata where decode = genericDecode options
instance encodeUserMetadata :: Encode UserMetadata where encode = genericEncode options



newtype Value = Value String
derive instance newtypeValue :: Newtype Value _
derive instance repGenericValue :: Generic Value _
instance showValue :: Show Value where show = genericShow
instance decodeValue :: Decode Value where decode = genericDecode options
instance encodeValue :: Encode Value where encode = genericEncode options



newtype VersionIdMarker = VersionIdMarker String
derive instance newtypeVersionIdMarker :: Newtype VersionIdMarker _
derive instance repGenericVersionIdMarker :: Generic VersionIdMarker _
instance showVersionIdMarker :: Show VersionIdMarker where show = genericShow
instance decodeVersionIdMarker :: Decode VersionIdMarker where decode = genericDecode options
instance encodeVersionIdMarker :: Encode VersionIdMarker where encode = genericEncode options



newtype VersioningConfiguration = VersioningConfiguration 
  { "MFADelete" :: NullOrUndefined (MFADelete)
  , "Status" :: NullOrUndefined (BucketVersioningStatus)
  }
derive instance newtypeVersioningConfiguration :: Newtype VersioningConfiguration _
derive instance repGenericVersioningConfiguration :: Generic VersioningConfiguration _
instance showVersioningConfiguration :: Show VersioningConfiguration where show = genericShow
instance decodeVersioningConfiguration :: Decode VersioningConfiguration where decode = genericDecode options
instance encodeVersioningConfiguration :: Encode VersioningConfiguration where encode = genericEncode options

-- | Constructs VersioningConfiguration from required parameters
newVersioningConfiguration :: VersioningConfiguration
newVersioningConfiguration  = VersioningConfiguration { "MFADelete": (NullOrUndefined Nothing), "Status": (NullOrUndefined Nothing) }

-- | Constructs VersioningConfiguration's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newVersioningConfiguration' :: ( { "MFADelete" :: NullOrUndefined (MFADelete) , "Status" :: NullOrUndefined (BucketVersioningStatus) } -> {"MFADelete" :: NullOrUndefined (MFADelete) , "Status" :: NullOrUndefined (BucketVersioningStatus) } ) -> VersioningConfiguration
newVersioningConfiguration'  customize = (VersioningConfiguration <<< customize) { "MFADelete": (NullOrUndefined Nothing), "Status": (NullOrUndefined Nothing) }



newtype WebsiteConfiguration = WebsiteConfiguration 
  { "ErrorDocument" :: NullOrUndefined (ErrorDocument)
  , "IndexDocument" :: NullOrUndefined (IndexDocument)
  , "RedirectAllRequestsTo" :: NullOrUndefined (RedirectAllRequestsTo)
  , "RoutingRules" :: NullOrUndefined (RoutingRules)
  }
derive instance newtypeWebsiteConfiguration :: Newtype WebsiteConfiguration _
derive instance repGenericWebsiteConfiguration :: Generic WebsiteConfiguration _
instance showWebsiteConfiguration :: Show WebsiteConfiguration where show = genericShow
instance decodeWebsiteConfiguration :: Decode WebsiteConfiguration where decode = genericDecode options
instance encodeWebsiteConfiguration :: Encode WebsiteConfiguration where encode = genericEncode options

-- | Constructs WebsiteConfiguration from required parameters
newWebsiteConfiguration :: WebsiteConfiguration
newWebsiteConfiguration  = WebsiteConfiguration { "ErrorDocument": (NullOrUndefined Nothing), "IndexDocument": (NullOrUndefined Nothing), "RedirectAllRequestsTo": (NullOrUndefined Nothing), "RoutingRules": (NullOrUndefined Nothing) }

-- | Constructs WebsiteConfiguration's fields from required parameters
--   This may be useful if you need to immediately overwrite some of the optional values
newWebsiteConfiguration' :: ( { "ErrorDocument" :: NullOrUndefined (ErrorDocument) , "IndexDocument" :: NullOrUndefined (IndexDocument) , "RedirectAllRequestsTo" :: NullOrUndefined (RedirectAllRequestsTo) , "RoutingRules" :: NullOrUndefined (RoutingRules) } -> {"ErrorDocument" :: NullOrUndefined (ErrorDocument) , "IndexDocument" :: NullOrUndefined (IndexDocument) , "RedirectAllRequestsTo" :: NullOrUndefined (RedirectAllRequestsTo) , "RoutingRules" :: NullOrUndefined (RoutingRules) } ) -> WebsiteConfiguration
newWebsiteConfiguration'  customize = (WebsiteConfiguration <<< customize) { "ErrorDocument": (NullOrUndefined Nothing), "IndexDocument": (NullOrUndefined Nothing), "RedirectAllRequestsTo": (NullOrUndefined Nothing), "RoutingRules": (NullOrUndefined Nothing) }



newtype WebsiteRedirectLocation = WebsiteRedirectLocation String
derive instance newtypeWebsiteRedirectLocation :: Newtype WebsiteRedirectLocation _
derive instance repGenericWebsiteRedirectLocation :: Generic WebsiteRedirectLocation _
instance showWebsiteRedirectLocation :: Show WebsiteRedirectLocation where show = genericShow
instance decodeWebsiteRedirectLocation :: Decode WebsiteRedirectLocation where decode = genericDecode options
instance encodeWebsiteRedirectLocation :: Encode WebsiteRedirectLocation where encode = genericEncode options

