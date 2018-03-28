## Module AWS.S3.Types

#### `options`

``` purescript
options :: Options
```

#### `AbortDate`

``` purescript
newtype AbortDate
  = AbortDate Timestamp
```

##### Instances
``` purescript
Newtype AbortDate _
Generic AbortDate _
Show AbortDate
Decode AbortDate
Encode AbortDate
```

#### `AbortIncompleteMultipartUpload`

``` purescript
newtype AbortIncompleteMultipartUpload
  = AbortIncompleteMultipartUpload { "DaysAfterInitiation" :: NullOrUndefined (DaysAfterInitiation) }
```

Specifies the days since the initiation of an Incomplete Multipart Upload that Lifecycle will wait before permanently removing all parts of the upload.

##### Instances
``` purescript
Newtype AbortIncompleteMultipartUpload _
Generic AbortIncompleteMultipartUpload _
Show AbortIncompleteMultipartUpload
Decode AbortIncompleteMultipartUpload
Encode AbortIncompleteMultipartUpload
```

#### `newAbortIncompleteMultipartUpload`

``` purescript
newAbortIncompleteMultipartUpload :: AbortIncompleteMultipartUpload
```

Constructs AbortIncompleteMultipartUpload from required parameters

#### `newAbortIncompleteMultipartUpload'`

``` purescript
newAbortIncompleteMultipartUpload' :: ({ "DaysAfterInitiation" :: NullOrUndefined (DaysAfterInitiation) } -> { "DaysAfterInitiation" :: NullOrUndefined (DaysAfterInitiation) }) -> AbortIncompleteMultipartUpload
```

Constructs AbortIncompleteMultipartUpload's fields from required parameters

#### `AbortMultipartUploadOutput`

``` purescript
newtype AbortMultipartUploadOutput
  = AbortMultipartUploadOutput { "RequestCharged" :: NullOrUndefined (RequestCharged) }
```

##### Instances
``` purescript
Newtype AbortMultipartUploadOutput _
Generic AbortMultipartUploadOutput _
Show AbortMultipartUploadOutput
Decode AbortMultipartUploadOutput
Encode AbortMultipartUploadOutput
```

#### `newAbortMultipartUploadOutput`

``` purescript
newAbortMultipartUploadOutput :: AbortMultipartUploadOutput
```

Constructs AbortMultipartUploadOutput from required parameters

#### `newAbortMultipartUploadOutput'`

``` purescript
newAbortMultipartUploadOutput' :: ({ "RequestCharged" :: NullOrUndefined (RequestCharged) } -> { "RequestCharged" :: NullOrUndefined (RequestCharged) }) -> AbortMultipartUploadOutput
```

Constructs AbortMultipartUploadOutput's fields from required parameters

#### `AbortMultipartUploadRequest`

``` purescript
newtype AbortMultipartUploadRequest
  = AbortMultipartUploadRequest { "Bucket" :: BucketName, "Key" :: ObjectKey, "UploadId" :: MultipartUploadId, "RequestPayer" :: NullOrUndefined (RequestPayer) }
```

##### Instances
``` purescript
Newtype AbortMultipartUploadRequest _
Generic AbortMultipartUploadRequest _
Show AbortMultipartUploadRequest
Decode AbortMultipartUploadRequest
Encode AbortMultipartUploadRequest
```

#### `newAbortMultipartUploadRequest`

``` purescript
newAbortMultipartUploadRequest :: BucketName -> ObjectKey -> MultipartUploadId -> AbortMultipartUploadRequest
```

Constructs AbortMultipartUploadRequest from required parameters

#### `newAbortMultipartUploadRequest'`

``` purescript
newAbortMultipartUploadRequest' :: BucketName -> ObjectKey -> MultipartUploadId -> ({ "Bucket" :: BucketName, "Key" :: ObjectKey, "UploadId" :: MultipartUploadId, "RequestPayer" :: NullOrUndefined (RequestPayer) } -> { "Bucket" :: BucketName, "Key" :: ObjectKey, "UploadId" :: MultipartUploadId, "RequestPayer" :: NullOrUndefined (RequestPayer) }) -> AbortMultipartUploadRequest
```

Constructs AbortMultipartUploadRequest's fields from required parameters

#### `AbortRuleId`

``` purescript
newtype AbortRuleId
  = AbortRuleId String
```

##### Instances
``` purescript
Newtype AbortRuleId _
Generic AbortRuleId _
Show AbortRuleId
Decode AbortRuleId
Encode AbortRuleId
```

#### `AccelerateConfiguration`

``` purescript
newtype AccelerateConfiguration
  = AccelerateConfiguration { "Status" :: NullOrUndefined (BucketAccelerateStatus) }
```

##### Instances
``` purescript
Newtype AccelerateConfiguration _
Generic AccelerateConfiguration _
Show AccelerateConfiguration
Decode AccelerateConfiguration
Encode AccelerateConfiguration
```

#### `newAccelerateConfiguration`

``` purescript
newAccelerateConfiguration :: AccelerateConfiguration
```

Constructs AccelerateConfiguration from required parameters

#### `newAccelerateConfiguration'`

``` purescript
newAccelerateConfiguration' :: ({ "Status" :: NullOrUndefined (BucketAccelerateStatus) } -> { "Status" :: NullOrUndefined (BucketAccelerateStatus) }) -> AccelerateConfiguration
```

Constructs AccelerateConfiguration's fields from required parameters

#### `AcceptRanges`

``` purescript
newtype AcceptRanges
  = AcceptRanges String
```

##### Instances
``` purescript
Newtype AcceptRanges _
Generic AcceptRanges _
Show AcceptRanges
Decode AcceptRanges
Encode AcceptRanges
```

#### `AccessControlPolicy`

``` purescript
newtype AccessControlPolicy
  = AccessControlPolicy { "Grants" :: NullOrUndefined (Grants), "Owner" :: NullOrUndefined (Owner) }
```

##### Instances
``` purescript
Newtype AccessControlPolicy _
Generic AccessControlPolicy _
Show AccessControlPolicy
Decode AccessControlPolicy
Encode AccessControlPolicy
```

#### `newAccessControlPolicy`

``` purescript
newAccessControlPolicy :: AccessControlPolicy
```

Constructs AccessControlPolicy from required parameters

#### `newAccessControlPolicy'`

``` purescript
newAccessControlPolicy' :: ({ "Grants" :: NullOrUndefined (Grants), "Owner" :: NullOrUndefined (Owner) } -> { "Grants" :: NullOrUndefined (Grants), "Owner" :: NullOrUndefined (Owner) }) -> AccessControlPolicy
```

Constructs AccessControlPolicy's fields from required parameters

#### `AccessControlTranslation`

``` purescript
newtype AccessControlTranslation
  = AccessControlTranslation { "Owner" :: OwnerOverride }
```

Container for information regarding the access control for replicas.

##### Instances
``` purescript
Newtype AccessControlTranslation _
Generic AccessControlTranslation _
Show AccessControlTranslation
Decode AccessControlTranslation
Encode AccessControlTranslation
```

#### `newAccessControlTranslation`

``` purescript
newAccessControlTranslation :: OwnerOverride -> AccessControlTranslation
```

Constructs AccessControlTranslation from required parameters

#### `newAccessControlTranslation'`

``` purescript
newAccessControlTranslation' :: OwnerOverride -> ({ "Owner" :: OwnerOverride } -> { "Owner" :: OwnerOverride }) -> AccessControlTranslation
```

Constructs AccessControlTranslation's fields from required parameters

#### `AccountId`

``` purescript
newtype AccountId
  = AccountId String
```

##### Instances
``` purescript
Newtype AccountId _
Generic AccountId _
Show AccountId
Decode AccountId
Encode AccountId
```

#### `AllowedHeader`

``` purescript
newtype AllowedHeader
  = AllowedHeader String
```

##### Instances
``` purescript
Newtype AllowedHeader _
Generic AllowedHeader _
Show AllowedHeader
Decode AllowedHeader
Encode AllowedHeader
```

#### `AllowedHeaders`

``` purescript
newtype AllowedHeaders
  = AllowedHeaders (Array AllowedHeader)
```

##### Instances
``` purescript
Newtype AllowedHeaders _
Generic AllowedHeaders _
Show AllowedHeaders
Decode AllowedHeaders
Encode AllowedHeaders
```

#### `AllowedMethod`

``` purescript
newtype AllowedMethod
  = AllowedMethod String
```

##### Instances
``` purescript
Newtype AllowedMethod _
Generic AllowedMethod _
Show AllowedMethod
Decode AllowedMethod
Encode AllowedMethod
```

#### `AllowedMethods`

``` purescript
newtype AllowedMethods
  = AllowedMethods (Array AllowedMethod)
```

##### Instances
``` purescript
Newtype AllowedMethods _
Generic AllowedMethods _
Show AllowedMethods
Decode AllowedMethods
Encode AllowedMethods
```

#### `AllowedOrigin`

``` purescript
newtype AllowedOrigin
  = AllowedOrigin String
```

##### Instances
``` purescript
Newtype AllowedOrigin _
Generic AllowedOrigin _
Show AllowedOrigin
Decode AllowedOrigin
Encode AllowedOrigin
```

#### `AllowedOrigins`

``` purescript
newtype AllowedOrigins
  = AllowedOrigins (Array AllowedOrigin)
```

##### Instances
``` purescript
Newtype AllowedOrigins _
Generic AllowedOrigins _
Show AllowedOrigins
Decode AllowedOrigins
Encode AllowedOrigins
```

#### `AnalyticsAndOperator`

``` purescript
newtype AnalyticsAndOperator
  = AnalyticsAndOperator { "Prefix" :: NullOrUndefined (Prefix), "Tags" :: NullOrUndefined (TagSet) }
```

##### Instances
``` purescript
Newtype AnalyticsAndOperator _
Generic AnalyticsAndOperator _
Show AnalyticsAndOperator
Decode AnalyticsAndOperator
Encode AnalyticsAndOperator
```

#### `newAnalyticsAndOperator`

``` purescript
newAnalyticsAndOperator :: AnalyticsAndOperator
```

Constructs AnalyticsAndOperator from required parameters

#### `newAnalyticsAndOperator'`

``` purescript
newAnalyticsAndOperator' :: ({ "Prefix" :: NullOrUndefined (Prefix), "Tags" :: NullOrUndefined (TagSet) } -> { "Prefix" :: NullOrUndefined (Prefix), "Tags" :: NullOrUndefined (TagSet) }) -> AnalyticsAndOperator
```

Constructs AnalyticsAndOperator's fields from required parameters

#### `AnalyticsConfiguration`

``` purescript
newtype AnalyticsConfiguration
  = AnalyticsConfiguration { "Id" :: AnalyticsId, "Filter" :: NullOrUndefined (AnalyticsFilter), "StorageClassAnalysis" :: StorageClassAnalysis }
```

##### Instances
``` purescript
Newtype AnalyticsConfiguration _
Generic AnalyticsConfiguration _
Show AnalyticsConfiguration
Decode AnalyticsConfiguration
Encode AnalyticsConfiguration
```

#### `newAnalyticsConfiguration`

``` purescript
newAnalyticsConfiguration :: AnalyticsId -> StorageClassAnalysis -> AnalyticsConfiguration
```

Constructs AnalyticsConfiguration from required parameters

#### `newAnalyticsConfiguration'`

``` purescript
newAnalyticsConfiguration' :: AnalyticsId -> StorageClassAnalysis -> ({ "Id" :: AnalyticsId, "Filter" :: NullOrUndefined (AnalyticsFilter), "StorageClassAnalysis" :: StorageClassAnalysis } -> { "Id" :: AnalyticsId, "Filter" :: NullOrUndefined (AnalyticsFilter), "StorageClassAnalysis" :: StorageClassAnalysis }) -> AnalyticsConfiguration
```

Constructs AnalyticsConfiguration's fields from required parameters

#### `AnalyticsConfigurationList`

``` purescript
newtype AnalyticsConfigurationList
  = AnalyticsConfigurationList (Array AnalyticsConfiguration)
```

##### Instances
``` purescript
Newtype AnalyticsConfigurationList _
Generic AnalyticsConfigurationList _
Show AnalyticsConfigurationList
Decode AnalyticsConfigurationList
Encode AnalyticsConfigurationList
```

#### `AnalyticsExportDestination`

``` purescript
newtype AnalyticsExportDestination
  = AnalyticsExportDestination { "S3BucketDestination" :: AnalyticsS3BucketDestination }
```

##### Instances
``` purescript
Newtype AnalyticsExportDestination _
Generic AnalyticsExportDestination _
Show AnalyticsExportDestination
Decode AnalyticsExportDestination
Encode AnalyticsExportDestination
```

#### `newAnalyticsExportDestination`

``` purescript
newAnalyticsExportDestination :: AnalyticsS3BucketDestination -> AnalyticsExportDestination
```

Constructs AnalyticsExportDestination from required parameters

#### `newAnalyticsExportDestination'`

``` purescript
newAnalyticsExportDestination' :: AnalyticsS3BucketDestination -> ({ "S3BucketDestination" :: AnalyticsS3BucketDestination } -> { "S3BucketDestination" :: AnalyticsS3BucketDestination }) -> AnalyticsExportDestination
```

Constructs AnalyticsExportDestination's fields from required parameters

#### `AnalyticsFilter`

``` purescript
newtype AnalyticsFilter
  = AnalyticsFilter { "Prefix" :: NullOrUndefined (Prefix), "Tag" :: NullOrUndefined (Tag), "And" :: NullOrUndefined (AnalyticsAndOperator) }
```

##### Instances
``` purescript
Newtype AnalyticsFilter _
Generic AnalyticsFilter _
Show AnalyticsFilter
Decode AnalyticsFilter
Encode AnalyticsFilter
```

#### `newAnalyticsFilter`

``` purescript
newAnalyticsFilter :: AnalyticsFilter
```

Constructs AnalyticsFilter from required parameters

#### `newAnalyticsFilter'`

``` purescript
newAnalyticsFilter' :: ({ "Prefix" :: NullOrUndefined (Prefix), "Tag" :: NullOrUndefined (Tag), "And" :: NullOrUndefined (AnalyticsAndOperator) } -> { "Prefix" :: NullOrUndefined (Prefix), "Tag" :: NullOrUndefined (Tag), "And" :: NullOrUndefined (AnalyticsAndOperator) }) -> AnalyticsFilter
```

Constructs AnalyticsFilter's fields from required parameters

#### `AnalyticsId`

``` purescript
newtype AnalyticsId
  = AnalyticsId String
```

##### Instances
``` purescript
Newtype AnalyticsId _
Generic AnalyticsId _
Show AnalyticsId
Decode AnalyticsId
Encode AnalyticsId
```

#### `AnalyticsS3BucketDestination`

``` purescript
newtype AnalyticsS3BucketDestination
  = AnalyticsS3BucketDestination { "Format" :: AnalyticsS3ExportFileFormat, "BucketAccountId" :: NullOrUndefined (AccountId), "Bucket" :: BucketName, "Prefix" :: NullOrUndefined (Prefix) }
```

##### Instances
``` purescript
Newtype AnalyticsS3BucketDestination _
Generic AnalyticsS3BucketDestination _
Show AnalyticsS3BucketDestination
Decode AnalyticsS3BucketDestination
Encode AnalyticsS3BucketDestination
```

#### `newAnalyticsS3BucketDestination`

``` purescript
newAnalyticsS3BucketDestination :: BucketName -> AnalyticsS3ExportFileFormat -> AnalyticsS3BucketDestination
```

Constructs AnalyticsS3BucketDestination from required parameters

#### `newAnalyticsS3BucketDestination'`

``` purescript
newAnalyticsS3BucketDestination' :: BucketName -> AnalyticsS3ExportFileFormat -> ({ "Format" :: AnalyticsS3ExportFileFormat, "BucketAccountId" :: NullOrUndefined (AccountId), "Bucket" :: BucketName, "Prefix" :: NullOrUndefined (Prefix) } -> { "Format" :: AnalyticsS3ExportFileFormat, "BucketAccountId" :: NullOrUndefined (AccountId), "Bucket" :: BucketName, "Prefix" :: NullOrUndefined (Prefix) }) -> AnalyticsS3BucketDestination
```

Constructs AnalyticsS3BucketDestination's fields from required parameters

#### `AnalyticsS3ExportFileFormat`

``` purescript
newtype AnalyticsS3ExportFileFormat
  = AnalyticsS3ExportFileFormat String
```

##### Instances
``` purescript
Newtype AnalyticsS3ExportFileFormat _
Generic AnalyticsS3ExportFileFormat _
Show AnalyticsS3ExportFileFormat
Decode AnalyticsS3ExportFileFormat
Encode AnalyticsS3ExportFileFormat
```

#### `Body`

``` purescript
newtype Body
  = Body String
```

##### Instances
``` purescript
Newtype Body _
Generic Body _
Show Body
Decode Body
Encode Body
```

#### `Bucket`

``` purescript
newtype Bucket
  = Bucket { "Name" :: NullOrUndefined (BucketName), "CreationDate" :: NullOrUndefined (CreationDate) }
```

##### Instances
``` purescript
Newtype Bucket _
Generic Bucket _
Show Bucket
Decode Bucket
Encode Bucket
```

#### `newBucket`

``` purescript
newBucket :: Bucket
```

Constructs Bucket from required parameters

#### `newBucket'`

``` purescript
newBucket' :: ({ "Name" :: NullOrUndefined (BucketName), "CreationDate" :: NullOrUndefined (CreationDate) } -> { "Name" :: NullOrUndefined (BucketName), "CreationDate" :: NullOrUndefined (CreationDate) }) -> Bucket
```

Constructs Bucket's fields from required parameters

#### `BucketAccelerateStatus`

``` purescript
newtype BucketAccelerateStatus
  = BucketAccelerateStatus String
```

##### Instances
``` purescript
Newtype BucketAccelerateStatus _
Generic BucketAccelerateStatus _
Show BucketAccelerateStatus
Decode BucketAccelerateStatus
Encode BucketAccelerateStatus
```

#### `BucketAlreadyExists`

``` purescript
newtype BucketAlreadyExists
  = BucketAlreadyExists NoArguments
```

The requested bucket name is not available. The bucket namespace is shared by all users of the system. Please select a different name and try again.

##### Instances
``` purescript
Newtype BucketAlreadyExists _
Generic BucketAlreadyExists _
Show BucketAlreadyExists
Decode BucketAlreadyExists
Encode BucketAlreadyExists
```

#### `BucketAlreadyOwnedByYou`

``` purescript
newtype BucketAlreadyOwnedByYou
  = BucketAlreadyOwnedByYou NoArguments
```

##### Instances
``` purescript
Newtype BucketAlreadyOwnedByYou _
Generic BucketAlreadyOwnedByYou _
Show BucketAlreadyOwnedByYou
Decode BucketAlreadyOwnedByYou
Encode BucketAlreadyOwnedByYou
```

#### `BucketCannedACL`

``` purescript
newtype BucketCannedACL
  = BucketCannedACL String
```

##### Instances
``` purescript
Newtype BucketCannedACL _
Generic BucketCannedACL _
Show BucketCannedACL
Decode BucketCannedACL
Encode BucketCannedACL
```

#### `BucketLifecycleConfiguration`

``` purescript
newtype BucketLifecycleConfiguration
  = BucketLifecycleConfiguration { "Rules" :: LifecycleRules }
```

##### Instances
``` purescript
Newtype BucketLifecycleConfiguration _
Generic BucketLifecycleConfiguration _
Show BucketLifecycleConfiguration
Decode BucketLifecycleConfiguration
Encode BucketLifecycleConfiguration
```

#### `newBucketLifecycleConfiguration`

``` purescript
newBucketLifecycleConfiguration :: LifecycleRules -> BucketLifecycleConfiguration
```

Constructs BucketLifecycleConfiguration from required parameters

#### `newBucketLifecycleConfiguration'`

``` purescript
newBucketLifecycleConfiguration' :: LifecycleRules -> ({ "Rules" :: LifecycleRules } -> { "Rules" :: LifecycleRules }) -> BucketLifecycleConfiguration
```

Constructs BucketLifecycleConfiguration's fields from required parameters

#### `BucketLocationConstraint`

``` purescript
newtype BucketLocationConstraint
  = BucketLocationConstraint String
```

##### Instances
``` purescript
Newtype BucketLocationConstraint _
Generic BucketLocationConstraint _
Show BucketLocationConstraint
Decode BucketLocationConstraint
Encode BucketLocationConstraint
```

#### `BucketLoggingStatus`

``` purescript
newtype BucketLoggingStatus
  = BucketLoggingStatus { "LoggingEnabled" :: NullOrUndefined (LoggingEnabled) }
```

##### Instances
``` purescript
Newtype BucketLoggingStatus _
Generic BucketLoggingStatus _
Show BucketLoggingStatus
Decode BucketLoggingStatus
Encode BucketLoggingStatus
```

#### `newBucketLoggingStatus`

``` purescript
newBucketLoggingStatus :: BucketLoggingStatus
```

Constructs BucketLoggingStatus from required parameters

#### `newBucketLoggingStatus'`

``` purescript
newBucketLoggingStatus' :: ({ "LoggingEnabled" :: NullOrUndefined (LoggingEnabled) } -> { "LoggingEnabled" :: NullOrUndefined (LoggingEnabled) }) -> BucketLoggingStatus
```

Constructs BucketLoggingStatus's fields from required parameters

#### `BucketLogsPermission`

``` purescript
newtype BucketLogsPermission
  = BucketLogsPermission String
```

##### Instances
``` purescript
Newtype BucketLogsPermission _
Generic BucketLogsPermission _
Show BucketLogsPermission
Decode BucketLogsPermission
Encode BucketLogsPermission
```

#### `BucketName`

``` purescript
newtype BucketName
  = BucketName String
```

##### Instances
``` purescript
Newtype BucketName _
Generic BucketName _
Show BucketName
Decode BucketName
Encode BucketName
```

#### `BucketVersioningStatus`

``` purescript
newtype BucketVersioningStatus
  = BucketVersioningStatus String
```

##### Instances
``` purescript
Newtype BucketVersioningStatus _
Generic BucketVersioningStatus _
Show BucketVersioningStatus
Decode BucketVersioningStatus
Encode BucketVersioningStatus
```

#### `Buckets`

``` purescript
newtype Buckets
  = Buckets (Array Bucket)
```

##### Instances
``` purescript
Newtype Buckets _
Generic Buckets _
Show Buckets
Decode Buckets
Encode Buckets
```

#### `CORSConfiguration`

``` purescript
newtype CORSConfiguration
  = CORSConfiguration { "CORSRules" :: CORSRules }
```

##### Instances
``` purescript
Newtype CORSConfiguration _
Generic CORSConfiguration _
Show CORSConfiguration
Decode CORSConfiguration
Encode CORSConfiguration
```

#### `newCORSConfiguration`

``` purescript
newCORSConfiguration :: CORSRules -> CORSConfiguration
```

Constructs CORSConfiguration from required parameters

#### `newCORSConfiguration'`

``` purescript
newCORSConfiguration' :: CORSRules -> ({ "CORSRules" :: CORSRules } -> { "CORSRules" :: CORSRules }) -> CORSConfiguration
```

Constructs CORSConfiguration's fields from required parameters

#### `CORSRule`

``` purescript
newtype CORSRule
  = CORSRule { "AllowedHeaders" :: NullOrUndefined (AllowedHeaders), "AllowedMethods" :: AllowedMethods, "AllowedOrigins" :: AllowedOrigins, "ExposeHeaders" :: NullOrUndefined (ExposeHeaders), "MaxAgeSeconds" :: NullOrUndefined (MaxAgeSeconds) }
```

##### Instances
``` purescript
Newtype CORSRule _
Generic CORSRule _
Show CORSRule
Decode CORSRule
Encode CORSRule
```

#### `newCORSRule`

``` purescript
newCORSRule :: AllowedMethods -> AllowedOrigins -> CORSRule
```

Constructs CORSRule from required parameters

#### `newCORSRule'`

``` purescript
newCORSRule' :: AllowedMethods -> AllowedOrigins -> ({ "AllowedHeaders" :: NullOrUndefined (AllowedHeaders), "AllowedMethods" :: AllowedMethods, "AllowedOrigins" :: AllowedOrigins, "ExposeHeaders" :: NullOrUndefined (ExposeHeaders), "MaxAgeSeconds" :: NullOrUndefined (MaxAgeSeconds) } -> { "AllowedHeaders" :: NullOrUndefined (AllowedHeaders), "AllowedMethods" :: AllowedMethods, "AllowedOrigins" :: AllowedOrigins, "ExposeHeaders" :: NullOrUndefined (ExposeHeaders), "MaxAgeSeconds" :: NullOrUndefined (MaxAgeSeconds) }) -> CORSRule
```

Constructs CORSRule's fields from required parameters

#### `CORSRules`

``` purescript
newtype CORSRules
  = CORSRules (Array CORSRule)
```

##### Instances
``` purescript
Newtype CORSRules _
Generic CORSRules _
Show CORSRules
Decode CORSRules
Encode CORSRules
```

#### `CSVInput`

``` purescript
newtype CSVInput
  = CSVInput { "FileHeaderInfo" :: NullOrUndefined (FileHeaderInfo), "Comments" :: NullOrUndefined (Comments), "QuoteEscapeCharacter" :: NullOrUndefined (QuoteEscapeCharacter), "RecordDelimiter" :: NullOrUndefined (RecordDelimiter), "FieldDelimiter" :: NullOrUndefined (FieldDelimiter), "QuoteCharacter" :: NullOrUndefined (QuoteCharacter) }
```

Describes how a CSV-formatted input object is formatted.

##### Instances
``` purescript
Newtype CSVInput _
Generic CSVInput _
Show CSVInput
Decode CSVInput
Encode CSVInput
```

#### `newCSVInput`

``` purescript
newCSVInput :: CSVInput
```

Constructs CSVInput from required parameters

#### `newCSVInput'`

``` purescript
newCSVInput' :: ({ "FileHeaderInfo" :: NullOrUndefined (FileHeaderInfo), "Comments" :: NullOrUndefined (Comments), "QuoteEscapeCharacter" :: NullOrUndefined (QuoteEscapeCharacter), "RecordDelimiter" :: NullOrUndefined (RecordDelimiter), "FieldDelimiter" :: NullOrUndefined (FieldDelimiter), "QuoteCharacter" :: NullOrUndefined (QuoteCharacter) } -> { "FileHeaderInfo" :: NullOrUndefined (FileHeaderInfo), "Comments" :: NullOrUndefined (Comments), "QuoteEscapeCharacter" :: NullOrUndefined (QuoteEscapeCharacter), "RecordDelimiter" :: NullOrUndefined (RecordDelimiter), "FieldDelimiter" :: NullOrUndefined (FieldDelimiter), "QuoteCharacter" :: NullOrUndefined (QuoteCharacter) }) -> CSVInput
```

Constructs CSVInput's fields from required parameters

#### `CSVOutput`

``` purescript
newtype CSVOutput
  = CSVOutput { "QuoteFields" :: NullOrUndefined (QuoteFields), "QuoteEscapeCharacter" :: NullOrUndefined (QuoteEscapeCharacter), "RecordDelimiter" :: NullOrUndefined (RecordDelimiter), "FieldDelimiter" :: NullOrUndefined (FieldDelimiter), "QuoteCharacter" :: NullOrUndefined (QuoteCharacter) }
```

Describes how CSV-formatted results are formatted.

##### Instances
``` purescript
Newtype CSVOutput _
Generic CSVOutput _
Show CSVOutput
Decode CSVOutput
Encode CSVOutput
```

#### `newCSVOutput`

``` purescript
newCSVOutput :: CSVOutput
```

Constructs CSVOutput from required parameters

#### `newCSVOutput'`

``` purescript
newCSVOutput' :: ({ "QuoteFields" :: NullOrUndefined (QuoteFields), "QuoteEscapeCharacter" :: NullOrUndefined (QuoteEscapeCharacter), "RecordDelimiter" :: NullOrUndefined (RecordDelimiter), "FieldDelimiter" :: NullOrUndefined (FieldDelimiter), "QuoteCharacter" :: NullOrUndefined (QuoteCharacter) } -> { "QuoteFields" :: NullOrUndefined (QuoteFields), "QuoteEscapeCharacter" :: NullOrUndefined (QuoteEscapeCharacter), "RecordDelimiter" :: NullOrUndefined (RecordDelimiter), "FieldDelimiter" :: NullOrUndefined (FieldDelimiter), "QuoteCharacter" :: NullOrUndefined (QuoteCharacter) }) -> CSVOutput
```

Constructs CSVOutput's fields from required parameters

#### `CacheControl`

``` purescript
newtype CacheControl
  = CacheControl String
```

##### Instances
``` purescript
Newtype CacheControl _
Generic CacheControl _
Show CacheControl
Decode CacheControl
Encode CacheControl
```

#### `CloudFunction`

``` purescript
newtype CloudFunction
  = CloudFunction String
```

##### Instances
``` purescript
Newtype CloudFunction _
Generic CloudFunction _
Show CloudFunction
Decode CloudFunction
Encode CloudFunction
```

#### `CloudFunctionConfiguration`

``` purescript
newtype CloudFunctionConfiguration
  = CloudFunctionConfiguration { "Id" :: NullOrUndefined (NotificationId), "Event" :: NullOrUndefined (Event), "Events" :: NullOrUndefined (EventList), "CloudFunction" :: NullOrUndefined (CloudFunction), "InvocationRole" :: NullOrUndefined (CloudFunctionInvocationRole) }
```

##### Instances
``` purescript
Newtype CloudFunctionConfiguration _
Generic CloudFunctionConfiguration _
Show CloudFunctionConfiguration
Decode CloudFunctionConfiguration
Encode CloudFunctionConfiguration
```

#### `newCloudFunctionConfiguration`

``` purescript
newCloudFunctionConfiguration :: CloudFunctionConfiguration
```

Constructs CloudFunctionConfiguration from required parameters

#### `newCloudFunctionConfiguration'`

``` purescript
newCloudFunctionConfiguration' :: ({ "Id" :: NullOrUndefined (NotificationId), "Event" :: NullOrUndefined (Event), "Events" :: NullOrUndefined (EventList), "CloudFunction" :: NullOrUndefined (CloudFunction), "InvocationRole" :: NullOrUndefined (CloudFunctionInvocationRole) } -> { "Id" :: NullOrUndefined (NotificationId), "Event" :: NullOrUndefined (Event), "Events" :: NullOrUndefined (EventList), "CloudFunction" :: NullOrUndefined (CloudFunction), "InvocationRole" :: NullOrUndefined (CloudFunctionInvocationRole) }) -> CloudFunctionConfiguration
```

Constructs CloudFunctionConfiguration's fields from required parameters

#### `CloudFunctionInvocationRole`

``` purescript
newtype CloudFunctionInvocationRole
  = CloudFunctionInvocationRole String
```

##### Instances
``` purescript
Newtype CloudFunctionInvocationRole _
Generic CloudFunctionInvocationRole _
Show CloudFunctionInvocationRole
Decode CloudFunctionInvocationRole
Encode CloudFunctionInvocationRole
```

#### `Code`

``` purescript
newtype Code
  = Code String
```

##### Instances
``` purescript
Newtype Code _
Generic Code _
Show Code
Decode Code
Encode Code
```

#### `Comments`

``` purescript
newtype Comments
  = Comments String
```

##### Instances
``` purescript
Newtype Comments _
Generic Comments _
Show Comments
Decode Comments
Encode Comments
```

#### `CommonPrefix`

``` purescript
newtype CommonPrefix
  = CommonPrefix { "Prefix" :: NullOrUndefined (Prefix) }
```

##### Instances
``` purescript
Newtype CommonPrefix _
Generic CommonPrefix _
Show CommonPrefix
Decode CommonPrefix
Encode CommonPrefix
```

#### `newCommonPrefix`

``` purescript
newCommonPrefix :: CommonPrefix
```

Constructs CommonPrefix from required parameters

#### `newCommonPrefix'`

``` purescript
newCommonPrefix' :: ({ "Prefix" :: NullOrUndefined (Prefix) } -> { "Prefix" :: NullOrUndefined (Prefix) }) -> CommonPrefix
```

Constructs CommonPrefix's fields from required parameters

#### `CommonPrefixList`

``` purescript
newtype CommonPrefixList
  = CommonPrefixList (Array CommonPrefix)
```

##### Instances
``` purescript
Newtype CommonPrefixList _
Generic CommonPrefixList _
Show CommonPrefixList
Decode CommonPrefixList
Encode CommonPrefixList
```

#### `CompleteMultipartUploadOutput`

``` purescript
newtype CompleteMultipartUploadOutput
  = CompleteMultipartUploadOutput { "Location" :: NullOrUndefined (Location), "Bucket" :: NullOrUndefined (BucketName), "Key" :: NullOrUndefined (ObjectKey), "Expiration" :: NullOrUndefined (Expiration), "ETag" :: NullOrUndefined (ETag), "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption), "VersionId" :: NullOrUndefined (ObjectVersionId), "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId), "RequestCharged" :: NullOrUndefined (RequestCharged) }
```

##### Instances
``` purescript
Newtype CompleteMultipartUploadOutput _
Generic CompleteMultipartUploadOutput _
Show CompleteMultipartUploadOutput
Decode CompleteMultipartUploadOutput
Encode CompleteMultipartUploadOutput
```

#### `newCompleteMultipartUploadOutput`

``` purescript
newCompleteMultipartUploadOutput :: CompleteMultipartUploadOutput
```

Constructs CompleteMultipartUploadOutput from required parameters

#### `newCompleteMultipartUploadOutput'`

``` purescript
newCompleteMultipartUploadOutput' :: ({ "Location" :: NullOrUndefined (Location), "Bucket" :: NullOrUndefined (BucketName), "Key" :: NullOrUndefined (ObjectKey), "Expiration" :: NullOrUndefined (Expiration), "ETag" :: NullOrUndefined (ETag), "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption), "VersionId" :: NullOrUndefined (ObjectVersionId), "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId), "RequestCharged" :: NullOrUndefined (RequestCharged) } -> { "Location" :: NullOrUndefined (Location), "Bucket" :: NullOrUndefined (BucketName), "Key" :: NullOrUndefined (ObjectKey), "Expiration" :: NullOrUndefined (Expiration), "ETag" :: NullOrUndefined (ETag), "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption), "VersionId" :: NullOrUndefined (ObjectVersionId), "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId), "RequestCharged" :: NullOrUndefined (RequestCharged) }) -> CompleteMultipartUploadOutput
```

Constructs CompleteMultipartUploadOutput's fields from required parameters

#### `CompleteMultipartUploadRequest`

``` purescript
newtype CompleteMultipartUploadRequest
  = CompleteMultipartUploadRequest { "Bucket" :: BucketName, "Key" :: ObjectKey, "MultipartUpload" :: NullOrUndefined (CompletedMultipartUpload), "UploadId" :: MultipartUploadId, "RequestPayer" :: NullOrUndefined (RequestPayer) }
```

##### Instances
``` purescript
Newtype CompleteMultipartUploadRequest _
Generic CompleteMultipartUploadRequest _
Show CompleteMultipartUploadRequest
Decode CompleteMultipartUploadRequest
Encode CompleteMultipartUploadRequest
```

#### `newCompleteMultipartUploadRequest`

``` purescript
newCompleteMultipartUploadRequest :: BucketName -> ObjectKey -> MultipartUploadId -> CompleteMultipartUploadRequest
```

Constructs CompleteMultipartUploadRequest from required parameters

#### `newCompleteMultipartUploadRequest'`

``` purescript
newCompleteMultipartUploadRequest' :: BucketName -> ObjectKey -> MultipartUploadId -> ({ "Bucket" :: BucketName, "Key" :: ObjectKey, "MultipartUpload" :: NullOrUndefined (CompletedMultipartUpload), "UploadId" :: MultipartUploadId, "RequestPayer" :: NullOrUndefined (RequestPayer) } -> { "Bucket" :: BucketName, "Key" :: ObjectKey, "MultipartUpload" :: NullOrUndefined (CompletedMultipartUpload), "UploadId" :: MultipartUploadId, "RequestPayer" :: NullOrUndefined (RequestPayer) }) -> CompleteMultipartUploadRequest
```

Constructs CompleteMultipartUploadRequest's fields from required parameters

#### `CompletedMultipartUpload`

``` purescript
newtype CompletedMultipartUpload
  = CompletedMultipartUpload { "Parts" :: NullOrUndefined (CompletedPartList) }
```

##### Instances
``` purescript
Newtype CompletedMultipartUpload _
Generic CompletedMultipartUpload _
Show CompletedMultipartUpload
Decode CompletedMultipartUpload
Encode CompletedMultipartUpload
```

#### `newCompletedMultipartUpload`

``` purescript
newCompletedMultipartUpload :: CompletedMultipartUpload
```

Constructs CompletedMultipartUpload from required parameters

#### `newCompletedMultipartUpload'`

``` purescript
newCompletedMultipartUpload' :: ({ "Parts" :: NullOrUndefined (CompletedPartList) } -> { "Parts" :: NullOrUndefined (CompletedPartList) }) -> CompletedMultipartUpload
```

Constructs CompletedMultipartUpload's fields from required parameters

#### `CompletedPart`

``` purescript
newtype CompletedPart
  = CompletedPart { "ETag" :: NullOrUndefined (ETag), "PartNumber" :: NullOrUndefined (PartNumber) }
```

##### Instances
``` purescript
Newtype CompletedPart _
Generic CompletedPart _
Show CompletedPart
Decode CompletedPart
Encode CompletedPart
```

#### `newCompletedPart`

``` purescript
newCompletedPart :: CompletedPart
```

Constructs CompletedPart from required parameters

#### `newCompletedPart'`

``` purescript
newCompletedPart' :: ({ "ETag" :: NullOrUndefined (ETag), "PartNumber" :: NullOrUndefined (PartNumber) } -> { "ETag" :: NullOrUndefined (ETag), "PartNumber" :: NullOrUndefined (PartNumber) }) -> CompletedPart
```

Constructs CompletedPart's fields from required parameters

#### `CompletedPartList`

``` purescript
newtype CompletedPartList
  = CompletedPartList (Array CompletedPart)
```

##### Instances
``` purescript
Newtype CompletedPartList _
Generic CompletedPartList _
Show CompletedPartList
Decode CompletedPartList
Encode CompletedPartList
```

#### `Condition`

``` purescript
newtype Condition
  = Condition { "HttpErrorCodeReturnedEquals" :: NullOrUndefined (HttpErrorCodeReturnedEquals), "KeyPrefixEquals" :: NullOrUndefined (KeyPrefixEquals) }
```

##### Instances
``` purescript
Newtype Condition _
Generic Condition _
Show Condition
Decode Condition
Encode Condition
```

#### `newCondition`

``` purescript
newCondition :: Condition
```

Constructs Condition from required parameters

#### `newCondition'`

``` purescript
newCondition' :: ({ "HttpErrorCodeReturnedEquals" :: NullOrUndefined (HttpErrorCodeReturnedEquals), "KeyPrefixEquals" :: NullOrUndefined (KeyPrefixEquals) } -> { "HttpErrorCodeReturnedEquals" :: NullOrUndefined (HttpErrorCodeReturnedEquals), "KeyPrefixEquals" :: NullOrUndefined (KeyPrefixEquals) }) -> Condition
```

Constructs Condition's fields from required parameters

#### `ConfirmRemoveSelfBucketAccess`

``` purescript
newtype ConfirmRemoveSelfBucketAccess
  = ConfirmRemoveSelfBucketAccess Boolean
```

##### Instances
``` purescript
Newtype ConfirmRemoveSelfBucketAccess _
Generic ConfirmRemoveSelfBucketAccess _
Show ConfirmRemoveSelfBucketAccess
Decode ConfirmRemoveSelfBucketAccess
Encode ConfirmRemoveSelfBucketAccess
```

#### `ContentDisposition`

``` purescript
newtype ContentDisposition
  = ContentDisposition String
```

##### Instances
``` purescript
Newtype ContentDisposition _
Generic ContentDisposition _
Show ContentDisposition
Decode ContentDisposition
Encode ContentDisposition
```

#### `ContentEncoding`

``` purescript
newtype ContentEncoding
  = ContentEncoding String
```

##### Instances
``` purescript
Newtype ContentEncoding _
Generic ContentEncoding _
Show ContentEncoding
Decode ContentEncoding
Encode ContentEncoding
```

#### `ContentLanguage`

``` purescript
newtype ContentLanguage
  = ContentLanguage String
```

##### Instances
``` purescript
Newtype ContentLanguage _
Generic ContentLanguage _
Show ContentLanguage
Decode ContentLanguage
Encode ContentLanguage
```

#### `ContentLength`

``` purescript
newtype ContentLength
  = ContentLength Number
```

##### Instances
``` purescript
Newtype ContentLength _
Generic ContentLength _
Show ContentLength
Decode ContentLength
Encode ContentLength
```

#### `ContentMD5`

``` purescript
newtype ContentMD5
  = ContentMD5 String
```

##### Instances
``` purescript
Newtype ContentMD5 _
Generic ContentMD5 _
Show ContentMD5
Decode ContentMD5
Encode ContentMD5
```

#### `ContentRange`

``` purescript
newtype ContentRange
  = ContentRange String
```

##### Instances
``` purescript
Newtype ContentRange _
Generic ContentRange _
Show ContentRange
Decode ContentRange
Encode ContentRange
```

#### `ContentType`

``` purescript
newtype ContentType
  = ContentType String
```

##### Instances
``` purescript
Newtype ContentType _
Generic ContentType _
Show ContentType
Decode ContentType
Encode ContentType
```

#### `CopyObjectOutput`

``` purescript
newtype CopyObjectOutput
  = CopyObjectOutput { "CopyObjectResult" :: NullOrUndefined (CopyObjectResult), "Expiration" :: NullOrUndefined (Expiration), "CopySourceVersionId" :: NullOrUndefined (CopySourceVersionId), "VersionId" :: NullOrUndefined (ObjectVersionId), "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption), "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm), "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5), "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId), "RequestCharged" :: NullOrUndefined (RequestCharged) }
```

##### Instances
``` purescript
Newtype CopyObjectOutput _
Generic CopyObjectOutput _
Show CopyObjectOutput
Decode CopyObjectOutput
Encode CopyObjectOutput
```

#### `newCopyObjectOutput`

``` purescript
newCopyObjectOutput :: CopyObjectOutput
```

Constructs CopyObjectOutput from required parameters

#### `newCopyObjectOutput'`

``` purescript
newCopyObjectOutput' :: ({ "CopyObjectResult" :: NullOrUndefined (CopyObjectResult), "Expiration" :: NullOrUndefined (Expiration), "CopySourceVersionId" :: NullOrUndefined (CopySourceVersionId), "VersionId" :: NullOrUndefined (ObjectVersionId), "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption), "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm), "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5), "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId), "RequestCharged" :: NullOrUndefined (RequestCharged) } -> { "CopyObjectResult" :: NullOrUndefined (CopyObjectResult), "Expiration" :: NullOrUndefined (Expiration), "CopySourceVersionId" :: NullOrUndefined (CopySourceVersionId), "VersionId" :: NullOrUndefined (ObjectVersionId), "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption), "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm), "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5), "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId), "RequestCharged" :: NullOrUndefined (RequestCharged) }) -> CopyObjectOutput
```

Constructs CopyObjectOutput's fields from required parameters

#### `CopyObjectRequest`

``` purescript
newtype CopyObjectRequest
  = CopyObjectRequest { "ACL" :: NullOrUndefined (ObjectCannedACL), "Bucket" :: BucketName, "CacheControl" :: NullOrUndefined (CacheControl), "ContentDisposition" :: NullOrUndefined (ContentDisposition), "ContentEncoding" :: NullOrUndefined (ContentEncoding), "ContentLanguage" :: NullOrUndefined (ContentLanguage), "ContentType" :: NullOrUndefined (ContentType), "CopySource" :: CopySource, "CopySourceIfMatch" :: NullOrUndefined (CopySourceIfMatch), "CopySourceIfModifiedSince" :: NullOrUndefined (CopySourceIfModifiedSince), "CopySourceIfNoneMatch" :: NullOrUndefined (CopySourceIfNoneMatch), "CopySourceIfUnmodifiedSince" :: NullOrUndefined (CopySourceIfUnmodifiedSince), "Expires" :: NullOrUndefined (Expires), "GrantFullControl" :: NullOrUndefined (GrantFullControl), "GrantRead" :: NullOrUndefined (GrantRead), "GrantReadACP" :: NullOrUndefined (GrantReadACP), "GrantWriteACP" :: NullOrUndefined (GrantWriteACP), "Key" :: ObjectKey, "Metadata" :: NullOrUndefined (Metadata), "MetadataDirective" :: NullOrUndefined (MetadataDirective), "TaggingDirective" :: NullOrUndefined (TaggingDirective), "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption), "StorageClass" :: NullOrUndefined (StorageClass), "WebsiteRedirectLocation" :: NullOrUndefined (WebsiteRedirectLocation), "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm), "SSECustomerKey" :: NullOrUndefined (SSECustomerKey), "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5), "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId), "CopySourceSSECustomerAlgorithm" :: NullOrUndefined (CopySourceSSECustomerAlgorithm), "CopySourceSSECustomerKey" :: NullOrUndefined (CopySourceSSECustomerKey), "CopySourceSSECustomerKeyMD5" :: NullOrUndefined (CopySourceSSECustomerKeyMD5), "RequestPayer" :: NullOrUndefined (RequestPayer), "Tagging" :: NullOrUndefined (TaggingHeader) }
```

##### Instances
``` purescript
Newtype CopyObjectRequest _
Generic CopyObjectRequest _
Show CopyObjectRequest
Decode CopyObjectRequest
Encode CopyObjectRequest
```

#### `newCopyObjectRequest`

``` purescript
newCopyObjectRequest :: BucketName -> CopySource -> ObjectKey -> CopyObjectRequest
```

Constructs CopyObjectRequest from required parameters

#### `newCopyObjectRequest'`

``` purescript
newCopyObjectRequest' :: BucketName -> CopySource -> ObjectKey -> ({ "ACL" :: NullOrUndefined (ObjectCannedACL), "Bucket" :: BucketName, "CacheControl" :: NullOrUndefined (CacheControl), "ContentDisposition" :: NullOrUndefined (ContentDisposition), "ContentEncoding" :: NullOrUndefined (ContentEncoding), "ContentLanguage" :: NullOrUndefined (ContentLanguage), "ContentType" :: NullOrUndefined (ContentType), "CopySource" :: CopySource, "CopySourceIfMatch" :: NullOrUndefined (CopySourceIfMatch), "CopySourceIfModifiedSince" :: NullOrUndefined (CopySourceIfModifiedSince), "CopySourceIfNoneMatch" :: NullOrUndefined (CopySourceIfNoneMatch), "CopySourceIfUnmodifiedSince" :: NullOrUndefined (CopySourceIfUnmodifiedSince), "Expires" :: NullOrUndefined (Expires), "GrantFullControl" :: NullOrUndefined (GrantFullControl), "GrantRead" :: NullOrUndefined (GrantRead), "GrantReadACP" :: NullOrUndefined (GrantReadACP), "GrantWriteACP" :: NullOrUndefined (GrantWriteACP), "Key" :: ObjectKey, "Metadata" :: NullOrUndefined (Metadata), "MetadataDirective" :: NullOrUndefined (MetadataDirective), "TaggingDirective" :: NullOrUndefined (TaggingDirective), "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption), "StorageClass" :: NullOrUndefined (StorageClass), "WebsiteRedirectLocation" :: NullOrUndefined (WebsiteRedirectLocation), "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm), "SSECustomerKey" :: NullOrUndefined (SSECustomerKey), "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5), "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId), "CopySourceSSECustomerAlgorithm" :: NullOrUndefined (CopySourceSSECustomerAlgorithm), "CopySourceSSECustomerKey" :: NullOrUndefined (CopySourceSSECustomerKey), "CopySourceSSECustomerKeyMD5" :: NullOrUndefined (CopySourceSSECustomerKeyMD5), "RequestPayer" :: NullOrUndefined (RequestPayer), "Tagging" :: NullOrUndefined (TaggingHeader) } -> { "ACL" :: NullOrUndefined (ObjectCannedACL), "Bucket" :: BucketName, "CacheControl" :: NullOrUndefined (CacheControl), "ContentDisposition" :: NullOrUndefined (ContentDisposition), "ContentEncoding" :: NullOrUndefined (ContentEncoding), "ContentLanguage" :: NullOrUndefined (ContentLanguage), "ContentType" :: NullOrUndefined (ContentType), "CopySource" :: CopySource, "CopySourceIfMatch" :: NullOrUndefined (CopySourceIfMatch), "CopySourceIfModifiedSince" :: NullOrUndefined (CopySourceIfModifiedSince), "CopySourceIfNoneMatch" :: NullOrUndefined (CopySourceIfNoneMatch), "CopySourceIfUnmodifiedSince" :: NullOrUndefined (CopySourceIfUnmodifiedSince), "Expires" :: NullOrUndefined (Expires), "GrantFullControl" :: NullOrUndefined (GrantFullControl), "GrantRead" :: NullOrUndefined (GrantRead), "GrantReadACP" :: NullOrUndefined (GrantReadACP), "GrantWriteACP" :: NullOrUndefined (GrantWriteACP), "Key" :: ObjectKey, "Metadata" :: NullOrUndefined (Metadata), "MetadataDirective" :: NullOrUndefined (MetadataDirective), "TaggingDirective" :: NullOrUndefined (TaggingDirective), "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption), "StorageClass" :: NullOrUndefined (StorageClass), "WebsiteRedirectLocation" :: NullOrUndefined (WebsiteRedirectLocation), "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm), "SSECustomerKey" :: NullOrUndefined (SSECustomerKey), "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5), "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId), "CopySourceSSECustomerAlgorithm" :: NullOrUndefined (CopySourceSSECustomerAlgorithm), "CopySourceSSECustomerKey" :: NullOrUndefined (CopySourceSSECustomerKey), "CopySourceSSECustomerKeyMD5" :: NullOrUndefined (CopySourceSSECustomerKeyMD5), "RequestPayer" :: NullOrUndefined (RequestPayer), "Tagging" :: NullOrUndefined (TaggingHeader) }) -> CopyObjectRequest
```

Constructs CopyObjectRequest's fields from required parameters

#### `CopyObjectResult`

``` purescript
newtype CopyObjectResult
  = CopyObjectResult { "ETag" :: NullOrUndefined (ETag), "LastModified" :: NullOrUndefined (LastModified) }
```

##### Instances
``` purescript
Newtype CopyObjectResult _
Generic CopyObjectResult _
Show CopyObjectResult
Decode CopyObjectResult
Encode CopyObjectResult
```

#### `newCopyObjectResult`

``` purescript
newCopyObjectResult :: CopyObjectResult
```

Constructs CopyObjectResult from required parameters

#### `newCopyObjectResult'`

``` purescript
newCopyObjectResult' :: ({ "ETag" :: NullOrUndefined (ETag), "LastModified" :: NullOrUndefined (LastModified) } -> { "ETag" :: NullOrUndefined (ETag), "LastModified" :: NullOrUndefined (LastModified) }) -> CopyObjectResult
```

Constructs CopyObjectResult's fields from required parameters

#### `CopyPartResult`

``` purescript
newtype CopyPartResult
  = CopyPartResult { "ETag" :: NullOrUndefined (ETag), "LastModified" :: NullOrUndefined (LastModified) }
```

##### Instances
``` purescript
Newtype CopyPartResult _
Generic CopyPartResult _
Show CopyPartResult
Decode CopyPartResult
Encode CopyPartResult
```

#### `newCopyPartResult`

``` purescript
newCopyPartResult :: CopyPartResult
```

Constructs CopyPartResult from required parameters

#### `newCopyPartResult'`

``` purescript
newCopyPartResult' :: ({ "ETag" :: NullOrUndefined (ETag), "LastModified" :: NullOrUndefined (LastModified) } -> { "ETag" :: NullOrUndefined (ETag), "LastModified" :: NullOrUndefined (LastModified) }) -> CopyPartResult
```

Constructs CopyPartResult's fields from required parameters

#### `CopySource`

``` purescript
newtype CopySource
  = CopySource String
```

##### Instances
``` purescript
Newtype CopySource _
Generic CopySource _
Show CopySource
Decode CopySource
Encode CopySource
```

#### `CopySourceIfMatch`

``` purescript
newtype CopySourceIfMatch
  = CopySourceIfMatch String
```

##### Instances
``` purescript
Newtype CopySourceIfMatch _
Generic CopySourceIfMatch _
Show CopySourceIfMatch
Decode CopySourceIfMatch
Encode CopySourceIfMatch
```

#### `CopySourceIfModifiedSince`

``` purescript
newtype CopySourceIfModifiedSince
  = CopySourceIfModifiedSince Timestamp
```

##### Instances
``` purescript
Newtype CopySourceIfModifiedSince _
Generic CopySourceIfModifiedSince _
Show CopySourceIfModifiedSince
Decode CopySourceIfModifiedSince
Encode CopySourceIfModifiedSince
```

#### `CopySourceIfNoneMatch`

``` purescript
newtype CopySourceIfNoneMatch
  = CopySourceIfNoneMatch String
```

##### Instances
``` purescript
Newtype CopySourceIfNoneMatch _
Generic CopySourceIfNoneMatch _
Show CopySourceIfNoneMatch
Decode CopySourceIfNoneMatch
Encode CopySourceIfNoneMatch
```

#### `CopySourceIfUnmodifiedSince`

``` purescript
newtype CopySourceIfUnmodifiedSince
  = CopySourceIfUnmodifiedSince Timestamp
```

##### Instances
``` purescript
Newtype CopySourceIfUnmodifiedSince _
Generic CopySourceIfUnmodifiedSince _
Show CopySourceIfUnmodifiedSince
Decode CopySourceIfUnmodifiedSince
Encode CopySourceIfUnmodifiedSince
```

#### `CopySourceRange`

``` purescript
newtype CopySourceRange
  = CopySourceRange String
```

##### Instances
``` purescript
Newtype CopySourceRange _
Generic CopySourceRange _
Show CopySourceRange
Decode CopySourceRange
Encode CopySourceRange
```

#### `CopySourceSSECustomerAlgorithm`

``` purescript
newtype CopySourceSSECustomerAlgorithm
  = CopySourceSSECustomerAlgorithm String
```

##### Instances
``` purescript
Newtype CopySourceSSECustomerAlgorithm _
Generic CopySourceSSECustomerAlgorithm _
Show CopySourceSSECustomerAlgorithm
Decode CopySourceSSECustomerAlgorithm
Encode CopySourceSSECustomerAlgorithm
```

#### `CopySourceSSECustomerKey`

``` purescript
newtype CopySourceSSECustomerKey
  = CopySourceSSECustomerKey String
```

##### Instances
``` purescript
Newtype CopySourceSSECustomerKey _
Generic CopySourceSSECustomerKey _
Show CopySourceSSECustomerKey
Decode CopySourceSSECustomerKey
Encode CopySourceSSECustomerKey
```

#### `CopySourceSSECustomerKeyMD5`

``` purescript
newtype CopySourceSSECustomerKeyMD5
  = CopySourceSSECustomerKeyMD5 String
```

##### Instances
``` purescript
Newtype CopySourceSSECustomerKeyMD5 _
Generic CopySourceSSECustomerKeyMD5 _
Show CopySourceSSECustomerKeyMD5
Decode CopySourceSSECustomerKeyMD5
Encode CopySourceSSECustomerKeyMD5
```

#### `CopySourceVersionId`

``` purescript
newtype CopySourceVersionId
  = CopySourceVersionId String
```

##### Instances
``` purescript
Newtype CopySourceVersionId _
Generic CopySourceVersionId _
Show CopySourceVersionId
Decode CopySourceVersionId
Encode CopySourceVersionId
```

#### `CreateBucketConfiguration`

``` purescript
newtype CreateBucketConfiguration
  = CreateBucketConfiguration { "LocationConstraint" :: NullOrUndefined (BucketLocationConstraint) }
```

##### Instances
``` purescript
Newtype CreateBucketConfiguration _
Generic CreateBucketConfiguration _
Show CreateBucketConfiguration
Decode CreateBucketConfiguration
Encode CreateBucketConfiguration
```

#### `newCreateBucketConfiguration`

``` purescript
newCreateBucketConfiguration :: CreateBucketConfiguration
```

Constructs CreateBucketConfiguration from required parameters

#### `newCreateBucketConfiguration'`

``` purescript
newCreateBucketConfiguration' :: ({ "LocationConstraint" :: NullOrUndefined (BucketLocationConstraint) } -> { "LocationConstraint" :: NullOrUndefined (BucketLocationConstraint) }) -> CreateBucketConfiguration
```

Constructs CreateBucketConfiguration's fields from required parameters

#### `CreateBucketOutput`

``` purescript
newtype CreateBucketOutput
  = CreateBucketOutput { "Location" :: NullOrUndefined (Location) }
```

##### Instances
``` purescript
Newtype CreateBucketOutput _
Generic CreateBucketOutput _
Show CreateBucketOutput
Decode CreateBucketOutput
Encode CreateBucketOutput
```

#### `newCreateBucketOutput`

``` purescript
newCreateBucketOutput :: CreateBucketOutput
```

Constructs CreateBucketOutput from required parameters

#### `newCreateBucketOutput'`

``` purescript
newCreateBucketOutput' :: ({ "Location" :: NullOrUndefined (Location) } -> { "Location" :: NullOrUndefined (Location) }) -> CreateBucketOutput
```

Constructs CreateBucketOutput's fields from required parameters

#### `CreateBucketRequest`

``` purescript
newtype CreateBucketRequest
  = CreateBucketRequest { "ACL" :: NullOrUndefined (BucketCannedACL), "Bucket" :: BucketName, "CreateBucketConfiguration" :: NullOrUndefined (CreateBucketConfiguration), "GrantFullControl" :: NullOrUndefined (GrantFullControl), "GrantRead" :: NullOrUndefined (GrantRead), "GrantReadACP" :: NullOrUndefined (GrantReadACP), "GrantWrite" :: NullOrUndefined (GrantWrite), "GrantWriteACP" :: NullOrUndefined (GrantWriteACP) }
```

##### Instances
``` purescript
Newtype CreateBucketRequest _
Generic CreateBucketRequest _
Show CreateBucketRequest
Decode CreateBucketRequest
Encode CreateBucketRequest
```

#### `newCreateBucketRequest`

``` purescript
newCreateBucketRequest :: BucketName -> CreateBucketRequest
```

Constructs CreateBucketRequest from required parameters

#### `newCreateBucketRequest'`

``` purescript
newCreateBucketRequest' :: BucketName -> ({ "ACL" :: NullOrUndefined (BucketCannedACL), "Bucket" :: BucketName, "CreateBucketConfiguration" :: NullOrUndefined (CreateBucketConfiguration), "GrantFullControl" :: NullOrUndefined (GrantFullControl), "GrantRead" :: NullOrUndefined (GrantRead), "GrantReadACP" :: NullOrUndefined (GrantReadACP), "GrantWrite" :: NullOrUndefined (GrantWrite), "GrantWriteACP" :: NullOrUndefined (GrantWriteACP) } -> { "ACL" :: NullOrUndefined (BucketCannedACL), "Bucket" :: BucketName, "CreateBucketConfiguration" :: NullOrUndefined (CreateBucketConfiguration), "GrantFullControl" :: NullOrUndefined (GrantFullControl), "GrantRead" :: NullOrUndefined (GrantRead), "GrantReadACP" :: NullOrUndefined (GrantReadACP), "GrantWrite" :: NullOrUndefined (GrantWrite), "GrantWriteACP" :: NullOrUndefined (GrantWriteACP) }) -> CreateBucketRequest
```

Constructs CreateBucketRequest's fields from required parameters

#### `CreateMultipartUploadOutput`

``` purescript
newtype CreateMultipartUploadOutput
  = CreateMultipartUploadOutput { "AbortDate" :: NullOrUndefined (AbortDate), "AbortRuleId" :: NullOrUndefined (AbortRuleId), "Bucket" :: NullOrUndefined (BucketName), "Key" :: NullOrUndefined (ObjectKey), "UploadId" :: NullOrUndefined (MultipartUploadId), "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption), "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm), "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5), "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId), "RequestCharged" :: NullOrUndefined (RequestCharged) }
```

##### Instances
``` purescript
Newtype CreateMultipartUploadOutput _
Generic CreateMultipartUploadOutput _
Show CreateMultipartUploadOutput
Decode CreateMultipartUploadOutput
Encode CreateMultipartUploadOutput
```

#### `newCreateMultipartUploadOutput`

``` purescript
newCreateMultipartUploadOutput :: CreateMultipartUploadOutput
```

Constructs CreateMultipartUploadOutput from required parameters

#### `newCreateMultipartUploadOutput'`

``` purescript
newCreateMultipartUploadOutput' :: ({ "AbortDate" :: NullOrUndefined (AbortDate), "AbortRuleId" :: NullOrUndefined (AbortRuleId), "Bucket" :: NullOrUndefined (BucketName), "Key" :: NullOrUndefined (ObjectKey), "UploadId" :: NullOrUndefined (MultipartUploadId), "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption), "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm), "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5), "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId), "RequestCharged" :: NullOrUndefined (RequestCharged) } -> { "AbortDate" :: NullOrUndefined (AbortDate), "AbortRuleId" :: NullOrUndefined (AbortRuleId), "Bucket" :: NullOrUndefined (BucketName), "Key" :: NullOrUndefined (ObjectKey), "UploadId" :: NullOrUndefined (MultipartUploadId), "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption), "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm), "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5), "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId), "RequestCharged" :: NullOrUndefined (RequestCharged) }) -> CreateMultipartUploadOutput
```

Constructs CreateMultipartUploadOutput's fields from required parameters

#### `CreateMultipartUploadRequest`

``` purescript
newtype CreateMultipartUploadRequest
  = CreateMultipartUploadRequest { "ACL" :: NullOrUndefined (ObjectCannedACL), "Bucket" :: BucketName, "CacheControl" :: NullOrUndefined (CacheControl), "ContentDisposition" :: NullOrUndefined (ContentDisposition), "ContentEncoding" :: NullOrUndefined (ContentEncoding), "ContentLanguage" :: NullOrUndefined (ContentLanguage), "ContentType" :: NullOrUndefined (ContentType), "Expires" :: NullOrUndefined (Expires), "GrantFullControl" :: NullOrUndefined (GrantFullControl), "GrantRead" :: NullOrUndefined (GrantRead), "GrantReadACP" :: NullOrUndefined (GrantReadACP), "GrantWriteACP" :: NullOrUndefined (GrantWriteACP), "Key" :: ObjectKey, "Metadata" :: NullOrUndefined (Metadata), "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption), "StorageClass" :: NullOrUndefined (StorageClass), "WebsiteRedirectLocation" :: NullOrUndefined (WebsiteRedirectLocation), "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm), "SSECustomerKey" :: NullOrUndefined (SSECustomerKey), "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5), "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId), "RequestPayer" :: NullOrUndefined (RequestPayer), "Tagging" :: NullOrUndefined (TaggingHeader) }
```

##### Instances
``` purescript
Newtype CreateMultipartUploadRequest _
Generic CreateMultipartUploadRequest _
Show CreateMultipartUploadRequest
Decode CreateMultipartUploadRequest
Encode CreateMultipartUploadRequest
```

#### `newCreateMultipartUploadRequest`

``` purescript
newCreateMultipartUploadRequest :: BucketName -> ObjectKey -> CreateMultipartUploadRequest
```

Constructs CreateMultipartUploadRequest from required parameters

#### `newCreateMultipartUploadRequest'`

``` purescript
newCreateMultipartUploadRequest' :: BucketName -> ObjectKey -> ({ "ACL" :: NullOrUndefined (ObjectCannedACL), "Bucket" :: BucketName, "CacheControl" :: NullOrUndefined (CacheControl), "ContentDisposition" :: NullOrUndefined (ContentDisposition), "ContentEncoding" :: NullOrUndefined (ContentEncoding), "ContentLanguage" :: NullOrUndefined (ContentLanguage), "ContentType" :: NullOrUndefined (ContentType), "Expires" :: NullOrUndefined (Expires), "GrantFullControl" :: NullOrUndefined (GrantFullControl), "GrantRead" :: NullOrUndefined (GrantRead), "GrantReadACP" :: NullOrUndefined (GrantReadACP), "GrantWriteACP" :: NullOrUndefined (GrantWriteACP), "Key" :: ObjectKey, "Metadata" :: NullOrUndefined (Metadata), "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption), "StorageClass" :: NullOrUndefined (StorageClass), "WebsiteRedirectLocation" :: NullOrUndefined (WebsiteRedirectLocation), "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm), "SSECustomerKey" :: NullOrUndefined (SSECustomerKey), "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5), "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId), "RequestPayer" :: NullOrUndefined (RequestPayer), "Tagging" :: NullOrUndefined (TaggingHeader) } -> { "ACL" :: NullOrUndefined (ObjectCannedACL), "Bucket" :: BucketName, "CacheControl" :: NullOrUndefined (CacheControl), "ContentDisposition" :: NullOrUndefined (ContentDisposition), "ContentEncoding" :: NullOrUndefined (ContentEncoding), "ContentLanguage" :: NullOrUndefined (ContentLanguage), "ContentType" :: NullOrUndefined (ContentType), "Expires" :: NullOrUndefined (Expires), "GrantFullControl" :: NullOrUndefined (GrantFullControl), "GrantRead" :: NullOrUndefined (GrantRead), "GrantReadACP" :: NullOrUndefined (GrantReadACP), "GrantWriteACP" :: NullOrUndefined (GrantWriteACP), "Key" :: ObjectKey, "Metadata" :: NullOrUndefined (Metadata), "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption), "StorageClass" :: NullOrUndefined (StorageClass), "WebsiteRedirectLocation" :: NullOrUndefined (WebsiteRedirectLocation), "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm), "SSECustomerKey" :: NullOrUndefined (SSECustomerKey), "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5), "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId), "RequestPayer" :: NullOrUndefined (RequestPayer), "Tagging" :: NullOrUndefined (TaggingHeader) }) -> CreateMultipartUploadRequest
```

Constructs CreateMultipartUploadRequest's fields from required parameters

#### `CreationDate`

``` purescript
newtype CreationDate
  = CreationDate Timestamp
```

##### Instances
``` purescript
Newtype CreationDate _
Generic CreationDate _
Show CreationDate
Decode CreationDate
Encode CreationDate
```

#### `Date`

``` purescript
newtype Date
  = Date Timestamp
```

##### Instances
``` purescript
Newtype Date _
Generic Date _
Show Date
Decode Date
Encode Date
```

#### `Days`

``` purescript
newtype Days
  = Days Int
```

##### Instances
``` purescript
Newtype Days _
Generic Days _
Show Days
Decode Days
Encode Days
```

#### `DaysAfterInitiation`

``` purescript
newtype DaysAfterInitiation
  = DaysAfterInitiation Int
```

##### Instances
``` purescript
Newtype DaysAfterInitiation _
Generic DaysAfterInitiation _
Show DaysAfterInitiation
Decode DaysAfterInitiation
Encode DaysAfterInitiation
```

#### `Delete`

``` purescript
newtype Delete
  = Delete { "Objects" :: ObjectIdentifierList, "Quiet" :: NullOrUndefined (Quiet) }
```

##### Instances
``` purescript
Newtype Delete _
Generic Delete _
Show Delete
Decode Delete
Encode Delete
```

#### `newDelete`

``` purescript
newDelete :: ObjectIdentifierList -> Delete
```

Constructs Delete from required parameters

#### `newDelete'`

``` purescript
newDelete' :: ObjectIdentifierList -> ({ "Objects" :: ObjectIdentifierList, "Quiet" :: NullOrUndefined (Quiet) } -> { "Objects" :: ObjectIdentifierList, "Quiet" :: NullOrUndefined (Quiet) }) -> Delete
```

Constructs Delete's fields from required parameters

#### `DeleteBucketAnalyticsConfigurationRequest`

``` purescript
newtype DeleteBucketAnalyticsConfigurationRequest
  = DeleteBucketAnalyticsConfigurationRequest { "Bucket" :: BucketName, "Id" :: AnalyticsId }
```

##### Instances
``` purescript
Newtype DeleteBucketAnalyticsConfigurationRequest _
Generic DeleteBucketAnalyticsConfigurationRequest _
Show DeleteBucketAnalyticsConfigurationRequest
Decode DeleteBucketAnalyticsConfigurationRequest
Encode DeleteBucketAnalyticsConfigurationRequest
```

#### `newDeleteBucketAnalyticsConfigurationRequest`

``` purescript
newDeleteBucketAnalyticsConfigurationRequest :: BucketName -> AnalyticsId -> DeleteBucketAnalyticsConfigurationRequest
```

Constructs DeleteBucketAnalyticsConfigurationRequest from required parameters

#### `newDeleteBucketAnalyticsConfigurationRequest'`

``` purescript
newDeleteBucketAnalyticsConfigurationRequest' :: BucketName -> AnalyticsId -> ({ "Bucket" :: BucketName, "Id" :: AnalyticsId } -> { "Bucket" :: BucketName, "Id" :: AnalyticsId }) -> DeleteBucketAnalyticsConfigurationRequest
```

Constructs DeleteBucketAnalyticsConfigurationRequest's fields from required parameters

#### `DeleteBucketCorsRequest`

``` purescript
newtype DeleteBucketCorsRequest
  = DeleteBucketCorsRequest { "Bucket" :: BucketName }
```

##### Instances
``` purescript
Newtype DeleteBucketCorsRequest _
Generic DeleteBucketCorsRequest _
Show DeleteBucketCorsRequest
Decode DeleteBucketCorsRequest
Encode DeleteBucketCorsRequest
```

#### `newDeleteBucketCorsRequest`

``` purescript
newDeleteBucketCorsRequest :: BucketName -> DeleteBucketCorsRequest
```

Constructs DeleteBucketCorsRequest from required parameters

#### `newDeleteBucketCorsRequest'`

``` purescript
newDeleteBucketCorsRequest' :: BucketName -> ({ "Bucket" :: BucketName } -> { "Bucket" :: BucketName }) -> DeleteBucketCorsRequest
```

Constructs DeleteBucketCorsRequest's fields from required parameters

#### `DeleteBucketEncryptionRequest`

``` purescript
newtype DeleteBucketEncryptionRequest
  = DeleteBucketEncryptionRequest { "Bucket" :: BucketName }
```

##### Instances
``` purescript
Newtype DeleteBucketEncryptionRequest _
Generic DeleteBucketEncryptionRequest _
Show DeleteBucketEncryptionRequest
Decode DeleteBucketEncryptionRequest
Encode DeleteBucketEncryptionRequest
```

#### `newDeleteBucketEncryptionRequest`

``` purescript
newDeleteBucketEncryptionRequest :: BucketName -> DeleteBucketEncryptionRequest
```

Constructs DeleteBucketEncryptionRequest from required parameters

#### `newDeleteBucketEncryptionRequest'`

``` purescript
newDeleteBucketEncryptionRequest' :: BucketName -> ({ "Bucket" :: BucketName } -> { "Bucket" :: BucketName }) -> DeleteBucketEncryptionRequest
```

Constructs DeleteBucketEncryptionRequest's fields from required parameters

#### `DeleteBucketInventoryConfigurationRequest`

``` purescript
newtype DeleteBucketInventoryConfigurationRequest
  = DeleteBucketInventoryConfigurationRequest { "Bucket" :: BucketName, "Id" :: InventoryId }
```

##### Instances
``` purescript
Newtype DeleteBucketInventoryConfigurationRequest _
Generic DeleteBucketInventoryConfigurationRequest _
Show DeleteBucketInventoryConfigurationRequest
Decode DeleteBucketInventoryConfigurationRequest
Encode DeleteBucketInventoryConfigurationRequest
```

#### `newDeleteBucketInventoryConfigurationRequest`

``` purescript
newDeleteBucketInventoryConfigurationRequest :: BucketName -> InventoryId -> DeleteBucketInventoryConfigurationRequest
```

Constructs DeleteBucketInventoryConfigurationRequest from required parameters

#### `newDeleteBucketInventoryConfigurationRequest'`

``` purescript
newDeleteBucketInventoryConfigurationRequest' :: BucketName -> InventoryId -> ({ "Bucket" :: BucketName, "Id" :: InventoryId } -> { "Bucket" :: BucketName, "Id" :: InventoryId }) -> DeleteBucketInventoryConfigurationRequest
```

Constructs DeleteBucketInventoryConfigurationRequest's fields from required parameters

#### `DeleteBucketLifecycleRequest`

``` purescript
newtype DeleteBucketLifecycleRequest
  = DeleteBucketLifecycleRequest { "Bucket" :: BucketName }
```

##### Instances
``` purescript
Newtype DeleteBucketLifecycleRequest _
Generic DeleteBucketLifecycleRequest _
Show DeleteBucketLifecycleRequest
Decode DeleteBucketLifecycleRequest
Encode DeleteBucketLifecycleRequest
```

#### `newDeleteBucketLifecycleRequest`

``` purescript
newDeleteBucketLifecycleRequest :: BucketName -> DeleteBucketLifecycleRequest
```

Constructs DeleteBucketLifecycleRequest from required parameters

#### `newDeleteBucketLifecycleRequest'`

``` purescript
newDeleteBucketLifecycleRequest' :: BucketName -> ({ "Bucket" :: BucketName } -> { "Bucket" :: BucketName }) -> DeleteBucketLifecycleRequest
```

Constructs DeleteBucketLifecycleRequest's fields from required parameters

#### `DeleteBucketMetricsConfigurationRequest`

``` purescript
newtype DeleteBucketMetricsConfigurationRequest
  = DeleteBucketMetricsConfigurationRequest { "Bucket" :: BucketName, "Id" :: MetricsId }
```

##### Instances
``` purescript
Newtype DeleteBucketMetricsConfigurationRequest _
Generic DeleteBucketMetricsConfigurationRequest _
Show DeleteBucketMetricsConfigurationRequest
Decode DeleteBucketMetricsConfigurationRequest
Encode DeleteBucketMetricsConfigurationRequest
```

#### `newDeleteBucketMetricsConfigurationRequest`

``` purescript
newDeleteBucketMetricsConfigurationRequest :: BucketName -> MetricsId -> DeleteBucketMetricsConfigurationRequest
```

Constructs DeleteBucketMetricsConfigurationRequest from required parameters

#### `newDeleteBucketMetricsConfigurationRequest'`

``` purescript
newDeleteBucketMetricsConfigurationRequest' :: BucketName -> MetricsId -> ({ "Bucket" :: BucketName, "Id" :: MetricsId } -> { "Bucket" :: BucketName, "Id" :: MetricsId }) -> DeleteBucketMetricsConfigurationRequest
```

Constructs DeleteBucketMetricsConfigurationRequest's fields from required parameters

#### `DeleteBucketPolicyRequest`

``` purescript
newtype DeleteBucketPolicyRequest
  = DeleteBucketPolicyRequest { "Bucket" :: BucketName }
```

##### Instances
``` purescript
Newtype DeleteBucketPolicyRequest _
Generic DeleteBucketPolicyRequest _
Show DeleteBucketPolicyRequest
Decode DeleteBucketPolicyRequest
Encode DeleteBucketPolicyRequest
```

#### `newDeleteBucketPolicyRequest`

``` purescript
newDeleteBucketPolicyRequest :: BucketName -> DeleteBucketPolicyRequest
```

Constructs DeleteBucketPolicyRequest from required parameters

#### `newDeleteBucketPolicyRequest'`

``` purescript
newDeleteBucketPolicyRequest' :: BucketName -> ({ "Bucket" :: BucketName } -> { "Bucket" :: BucketName }) -> DeleteBucketPolicyRequest
```

Constructs DeleteBucketPolicyRequest's fields from required parameters

#### `DeleteBucketReplicationRequest`

``` purescript
newtype DeleteBucketReplicationRequest
  = DeleteBucketReplicationRequest { "Bucket" :: BucketName }
```

##### Instances
``` purescript
Newtype DeleteBucketReplicationRequest _
Generic DeleteBucketReplicationRequest _
Show DeleteBucketReplicationRequest
Decode DeleteBucketReplicationRequest
Encode DeleteBucketReplicationRequest
```

#### `newDeleteBucketReplicationRequest`

``` purescript
newDeleteBucketReplicationRequest :: BucketName -> DeleteBucketReplicationRequest
```

Constructs DeleteBucketReplicationRequest from required parameters

#### `newDeleteBucketReplicationRequest'`

``` purescript
newDeleteBucketReplicationRequest' :: BucketName -> ({ "Bucket" :: BucketName } -> { "Bucket" :: BucketName }) -> DeleteBucketReplicationRequest
```

Constructs DeleteBucketReplicationRequest's fields from required parameters

#### `DeleteBucketRequest`

``` purescript
newtype DeleteBucketRequest
  = DeleteBucketRequest { "Bucket" :: BucketName }
```

##### Instances
``` purescript
Newtype DeleteBucketRequest _
Generic DeleteBucketRequest _
Show DeleteBucketRequest
Decode DeleteBucketRequest
Encode DeleteBucketRequest
```

#### `newDeleteBucketRequest`

``` purescript
newDeleteBucketRequest :: BucketName -> DeleteBucketRequest
```

Constructs DeleteBucketRequest from required parameters

#### `newDeleteBucketRequest'`

``` purescript
newDeleteBucketRequest' :: BucketName -> ({ "Bucket" :: BucketName } -> { "Bucket" :: BucketName }) -> DeleteBucketRequest
```

Constructs DeleteBucketRequest's fields from required parameters

#### `DeleteBucketTaggingRequest`

``` purescript
newtype DeleteBucketTaggingRequest
  = DeleteBucketTaggingRequest { "Bucket" :: BucketName }
```

##### Instances
``` purescript
Newtype DeleteBucketTaggingRequest _
Generic DeleteBucketTaggingRequest _
Show DeleteBucketTaggingRequest
Decode DeleteBucketTaggingRequest
Encode DeleteBucketTaggingRequest
```

#### `newDeleteBucketTaggingRequest`

``` purescript
newDeleteBucketTaggingRequest :: BucketName -> DeleteBucketTaggingRequest
```

Constructs DeleteBucketTaggingRequest from required parameters

#### `newDeleteBucketTaggingRequest'`

``` purescript
newDeleteBucketTaggingRequest' :: BucketName -> ({ "Bucket" :: BucketName } -> { "Bucket" :: BucketName }) -> DeleteBucketTaggingRequest
```

Constructs DeleteBucketTaggingRequest's fields from required parameters

#### `DeleteBucketWebsiteRequest`

``` purescript
newtype DeleteBucketWebsiteRequest
  = DeleteBucketWebsiteRequest { "Bucket" :: BucketName }
```

##### Instances
``` purescript
Newtype DeleteBucketWebsiteRequest _
Generic DeleteBucketWebsiteRequest _
Show DeleteBucketWebsiteRequest
Decode DeleteBucketWebsiteRequest
Encode DeleteBucketWebsiteRequest
```

#### `newDeleteBucketWebsiteRequest`

``` purescript
newDeleteBucketWebsiteRequest :: BucketName -> DeleteBucketWebsiteRequest
```

Constructs DeleteBucketWebsiteRequest from required parameters

#### `newDeleteBucketWebsiteRequest'`

``` purescript
newDeleteBucketWebsiteRequest' :: BucketName -> ({ "Bucket" :: BucketName } -> { "Bucket" :: BucketName }) -> DeleteBucketWebsiteRequest
```

Constructs DeleteBucketWebsiteRequest's fields from required parameters

#### `DeleteMarker`

``` purescript
newtype DeleteMarker
  = DeleteMarker Boolean
```

##### Instances
``` purescript
Newtype DeleteMarker _
Generic DeleteMarker _
Show DeleteMarker
Decode DeleteMarker
Encode DeleteMarker
```

#### `DeleteMarkerEntry`

``` purescript
newtype DeleteMarkerEntry
  = DeleteMarkerEntry { "Owner" :: NullOrUndefined (Owner), "Key" :: NullOrUndefined (ObjectKey), "VersionId" :: NullOrUndefined (ObjectVersionId), "IsLatest" :: NullOrUndefined (IsLatest), "LastModified" :: NullOrUndefined (LastModified) }
```

##### Instances
``` purescript
Newtype DeleteMarkerEntry _
Generic DeleteMarkerEntry _
Show DeleteMarkerEntry
Decode DeleteMarkerEntry
Encode DeleteMarkerEntry
```

#### `newDeleteMarkerEntry`

``` purescript
newDeleteMarkerEntry :: DeleteMarkerEntry
```

Constructs DeleteMarkerEntry from required parameters

#### `newDeleteMarkerEntry'`

``` purescript
newDeleteMarkerEntry' :: ({ "Owner" :: NullOrUndefined (Owner), "Key" :: NullOrUndefined (ObjectKey), "VersionId" :: NullOrUndefined (ObjectVersionId), "IsLatest" :: NullOrUndefined (IsLatest), "LastModified" :: NullOrUndefined (LastModified) } -> { "Owner" :: NullOrUndefined (Owner), "Key" :: NullOrUndefined (ObjectKey), "VersionId" :: NullOrUndefined (ObjectVersionId), "IsLatest" :: NullOrUndefined (IsLatest), "LastModified" :: NullOrUndefined (LastModified) }) -> DeleteMarkerEntry
```

Constructs DeleteMarkerEntry's fields from required parameters

#### `DeleteMarkerVersionId`

``` purescript
newtype DeleteMarkerVersionId
  = DeleteMarkerVersionId String
```

##### Instances
``` purescript
Newtype DeleteMarkerVersionId _
Generic DeleteMarkerVersionId _
Show DeleteMarkerVersionId
Decode DeleteMarkerVersionId
Encode DeleteMarkerVersionId
```

#### `DeleteMarkers`

``` purescript
newtype DeleteMarkers
  = DeleteMarkers (Array DeleteMarkerEntry)
```

##### Instances
``` purescript
Newtype DeleteMarkers _
Generic DeleteMarkers _
Show DeleteMarkers
Decode DeleteMarkers
Encode DeleteMarkers
```

#### `DeleteObjectOutput`

``` purescript
newtype DeleteObjectOutput
  = DeleteObjectOutput { "DeleteMarker" :: NullOrUndefined (DeleteMarker), "VersionId" :: NullOrUndefined (ObjectVersionId), "RequestCharged" :: NullOrUndefined (RequestCharged) }
```

##### Instances
``` purescript
Newtype DeleteObjectOutput _
Generic DeleteObjectOutput _
Show DeleteObjectOutput
Decode DeleteObjectOutput
Encode DeleteObjectOutput
```

#### `newDeleteObjectOutput`

``` purescript
newDeleteObjectOutput :: DeleteObjectOutput
```

Constructs DeleteObjectOutput from required parameters

#### `newDeleteObjectOutput'`

``` purescript
newDeleteObjectOutput' :: ({ "DeleteMarker" :: NullOrUndefined (DeleteMarker), "VersionId" :: NullOrUndefined (ObjectVersionId), "RequestCharged" :: NullOrUndefined (RequestCharged) } -> { "DeleteMarker" :: NullOrUndefined (DeleteMarker), "VersionId" :: NullOrUndefined (ObjectVersionId), "RequestCharged" :: NullOrUndefined (RequestCharged) }) -> DeleteObjectOutput
```

Constructs DeleteObjectOutput's fields from required parameters

#### `DeleteObjectRequest`

``` purescript
newtype DeleteObjectRequest
  = DeleteObjectRequest { "Bucket" :: BucketName, "Key" :: ObjectKey, "MFA" :: NullOrUndefined (MFA), "VersionId" :: NullOrUndefined (ObjectVersionId), "RequestPayer" :: NullOrUndefined (RequestPayer) }
```

##### Instances
``` purescript
Newtype DeleteObjectRequest _
Generic DeleteObjectRequest _
Show DeleteObjectRequest
Decode DeleteObjectRequest
Encode DeleteObjectRequest
```

#### `newDeleteObjectRequest`

``` purescript
newDeleteObjectRequest :: BucketName -> ObjectKey -> DeleteObjectRequest
```

Constructs DeleteObjectRequest from required parameters

#### `newDeleteObjectRequest'`

``` purescript
newDeleteObjectRequest' :: BucketName -> ObjectKey -> ({ "Bucket" :: BucketName, "Key" :: ObjectKey, "MFA" :: NullOrUndefined (MFA), "VersionId" :: NullOrUndefined (ObjectVersionId), "RequestPayer" :: NullOrUndefined (RequestPayer) } -> { "Bucket" :: BucketName, "Key" :: ObjectKey, "MFA" :: NullOrUndefined (MFA), "VersionId" :: NullOrUndefined (ObjectVersionId), "RequestPayer" :: NullOrUndefined (RequestPayer) }) -> DeleteObjectRequest
```

Constructs DeleteObjectRequest's fields from required parameters

#### `DeleteObjectTaggingOutput`

``` purescript
newtype DeleteObjectTaggingOutput
  = DeleteObjectTaggingOutput { "VersionId" :: NullOrUndefined (ObjectVersionId) }
```

##### Instances
``` purescript
Newtype DeleteObjectTaggingOutput _
Generic DeleteObjectTaggingOutput _
Show DeleteObjectTaggingOutput
Decode DeleteObjectTaggingOutput
Encode DeleteObjectTaggingOutput
```

#### `newDeleteObjectTaggingOutput`

``` purescript
newDeleteObjectTaggingOutput :: DeleteObjectTaggingOutput
```

Constructs DeleteObjectTaggingOutput from required parameters

#### `newDeleteObjectTaggingOutput'`

``` purescript
newDeleteObjectTaggingOutput' :: ({ "VersionId" :: NullOrUndefined (ObjectVersionId) } -> { "VersionId" :: NullOrUndefined (ObjectVersionId) }) -> DeleteObjectTaggingOutput
```

Constructs DeleteObjectTaggingOutput's fields from required parameters

#### `DeleteObjectTaggingRequest`

``` purescript
newtype DeleteObjectTaggingRequest
  = DeleteObjectTaggingRequest { "Bucket" :: BucketName, "Key" :: ObjectKey, "VersionId" :: NullOrUndefined (ObjectVersionId) }
```

##### Instances
``` purescript
Newtype DeleteObjectTaggingRequest _
Generic DeleteObjectTaggingRequest _
Show DeleteObjectTaggingRequest
Decode DeleteObjectTaggingRequest
Encode DeleteObjectTaggingRequest
```

#### `newDeleteObjectTaggingRequest`

``` purescript
newDeleteObjectTaggingRequest :: BucketName -> ObjectKey -> DeleteObjectTaggingRequest
```

Constructs DeleteObjectTaggingRequest from required parameters

#### `newDeleteObjectTaggingRequest'`

``` purescript
newDeleteObjectTaggingRequest' :: BucketName -> ObjectKey -> ({ "Bucket" :: BucketName, "Key" :: ObjectKey, "VersionId" :: NullOrUndefined (ObjectVersionId) } -> { "Bucket" :: BucketName, "Key" :: ObjectKey, "VersionId" :: NullOrUndefined (ObjectVersionId) }) -> DeleteObjectTaggingRequest
```

Constructs DeleteObjectTaggingRequest's fields from required parameters

#### `DeleteObjectsOutput`

``` purescript
newtype DeleteObjectsOutput
  = DeleteObjectsOutput { "Deleted" :: NullOrUndefined (DeletedObjects), "RequestCharged" :: NullOrUndefined (RequestCharged), "Errors" :: NullOrUndefined (Errors) }
```

##### Instances
``` purescript
Newtype DeleteObjectsOutput _
Generic DeleteObjectsOutput _
Show DeleteObjectsOutput
Decode DeleteObjectsOutput
Encode DeleteObjectsOutput
```

#### `newDeleteObjectsOutput`

``` purescript
newDeleteObjectsOutput :: DeleteObjectsOutput
```

Constructs DeleteObjectsOutput from required parameters

#### `newDeleteObjectsOutput'`

``` purescript
newDeleteObjectsOutput' :: ({ "Deleted" :: NullOrUndefined (DeletedObjects), "RequestCharged" :: NullOrUndefined (RequestCharged), "Errors" :: NullOrUndefined (Errors) } -> { "Deleted" :: NullOrUndefined (DeletedObjects), "RequestCharged" :: NullOrUndefined (RequestCharged), "Errors" :: NullOrUndefined (Errors) }) -> DeleteObjectsOutput
```

Constructs DeleteObjectsOutput's fields from required parameters

#### `DeleteObjectsRequest`

``` purescript
newtype DeleteObjectsRequest
  = DeleteObjectsRequest { "Bucket" :: BucketName, "Delete" :: Delete, "MFA" :: NullOrUndefined (MFA), "RequestPayer" :: NullOrUndefined (RequestPayer) }
```

##### Instances
``` purescript
Newtype DeleteObjectsRequest _
Generic DeleteObjectsRequest _
Show DeleteObjectsRequest
Decode DeleteObjectsRequest
Encode DeleteObjectsRequest
```

#### `newDeleteObjectsRequest`

``` purescript
newDeleteObjectsRequest :: BucketName -> Delete -> DeleteObjectsRequest
```

Constructs DeleteObjectsRequest from required parameters

#### `newDeleteObjectsRequest'`

``` purescript
newDeleteObjectsRequest' :: BucketName -> Delete -> ({ "Bucket" :: BucketName, "Delete" :: Delete, "MFA" :: NullOrUndefined (MFA), "RequestPayer" :: NullOrUndefined (RequestPayer) } -> { "Bucket" :: BucketName, "Delete" :: Delete, "MFA" :: NullOrUndefined (MFA), "RequestPayer" :: NullOrUndefined (RequestPayer) }) -> DeleteObjectsRequest
```

Constructs DeleteObjectsRequest's fields from required parameters

#### `DeletedObject`

``` purescript
newtype DeletedObject
  = DeletedObject { "Key" :: NullOrUndefined (ObjectKey), "VersionId" :: NullOrUndefined (ObjectVersionId), "DeleteMarker" :: NullOrUndefined (DeleteMarker), "DeleteMarkerVersionId" :: NullOrUndefined (DeleteMarkerVersionId) }
```

##### Instances
``` purescript
Newtype DeletedObject _
Generic DeletedObject _
Show DeletedObject
Decode DeletedObject
Encode DeletedObject
```

#### `newDeletedObject`

``` purescript
newDeletedObject :: DeletedObject
```

Constructs DeletedObject from required parameters

#### `newDeletedObject'`

``` purescript
newDeletedObject' :: ({ "Key" :: NullOrUndefined (ObjectKey), "VersionId" :: NullOrUndefined (ObjectVersionId), "DeleteMarker" :: NullOrUndefined (DeleteMarker), "DeleteMarkerVersionId" :: NullOrUndefined (DeleteMarkerVersionId) } -> { "Key" :: NullOrUndefined (ObjectKey), "VersionId" :: NullOrUndefined (ObjectVersionId), "DeleteMarker" :: NullOrUndefined (DeleteMarker), "DeleteMarkerVersionId" :: NullOrUndefined (DeleteMarkerVersionId) }) -> DeletedObject
```

Constructs DeletedObject's fields from required parameters

#### `DeletedObjects`

``` purescript
newtype DeletedObjects
  = DeletedObjects (Array DeletedObject)
```

##### Instances
``` purescript
Newtype DeletedObjects _
Generic DeletedObjects _
Show DeletedObjects
Decode DeletedObjects
Encode DeletedObjects
```

#### `Delimiter`

``` purescript
newtype Delimiter
  = Delimiter String
```

##### Instances
``` purescript
Newtype Delimiter _
Generic Delimiter _
Show Delimiter
Decode Delimiter
Encode Delimiter
```

#### `Description`

``` purescript
newtype Description
  = Description String
```

##### Instances
``` purescript
Newtype Description _
Generic Description _
Show Description
Decode Description
Encode Description
```

#### `Destination`

``` purescript
newtype Destination
  = Destination { "Bucket" :: BucketName, "Account" :: NullOrUndefined (AccountId), "StorageClass" :: NullOrUndefined (StorageClass), "AccessControlTranslation" :: NullOrUndefined (AccessControlTranslation), "EncryptionConfiguration" :: NullOrUndefined (EncryptionConfiguration) }
```

Container for replication destination information.

##### Instances
``` purescript
Newtype Destination _
Generic Destination _
Show Destination
Decode Destination
Encode Destination
```

#### `newDestination`

``` purescript
newDestination :: BucketName -> Destination
```

Constructs Destination from required parameters

#### `newDestination'`

``` purescript
newDestination' :: BucketName -> ({ "Bucket" :: BucketName, "Account" :: NullOrUndefined (AccountId), "StorageClass" :: NullOrUndefined (StorageClass), "AccessControlTranslation" :: NullOrUndefined (AccessControlTranslation), "EncryptionConfiguration" :: NullOrUndefined (EncryptionConfiguration) } -> { "Bucket" :: BucketName, "Account" :: NullOrUndefined (AccountId), "StorageClass" :: NullOrUndefined (StorageClass), "AccessControlTranslation" :: NullOrUndefined (AccessControlTranslation), "EncryptionConfiguration" :: NullOrUndefined (EncryptionConfiguration) }) -> Destination
```

Constructs Destination's fields from required parameters

#### `DisplayName`

``` purescript
newtype DisplayName
  = DisplayName String
```

##### Instances
``` purescript
Newtype DisplayName _
Generic DisplayName _
Show DisplayName
Decode DisplayName
Encode DisplayName
```

#### `ETag`

``` purescript
newtype ETag
  = ETag String
```

##### Instances
``` purescript
Newtype ETag _
Generic ETag _
Show ETag
Decode ETag
Encode ETag
```

#### `EmailAddress`

``` purescript
newtype EmailAddress
  = EmailAddress String
```

##### Instances
``` purescript
Newtype EmailAddress _
Generic EmailAddress _
Show EmailAddress
Decode EmailAddress
Encode EmailAddress
```

#### `EncodingType`

``` purescript
newtype EncodingType
  = EncodingType String
```

Requests Amazon S3 to encode the object keys in the response and specifies the encoding method to use. An object key may contain any Unicode character; however, XML 1.0 parser cannot parse some characters, such as characters with an ASCII value from 0 to 10. For characters that are not supported in XML 1.0, you can add this parameter to request that Amazon S3 encode the keys in the response.

##### Instances
``` purescript
Newtype EncodingType _
Generic EncodingType _
Show EncodingType
Decode EncodingType
Encode EncodingType
```

#### `Encryption`

``` purescript
newtype Encryption
  = Encryption { "EncryptionType" :: ServerSideEncryption, "KMSKeyId" :: NullOrUndefined (SSEKMSKeyId), "KMSContext" :: NullOrUndefined (KMSContext) }
```

Describes the server-side encryption that will be applied to the restore results.

##### Instances
``` purescript
Newtype Encryption _
Generic Encryption _
Show Encryption
Decode Encryption
Encode Encryption
```

#### `newEncryption`

``` purescript
newEncryption :: ServerSideEncryption -> Encryption
```

Constructs Encryption from required parameters

#### `newEncryption'`

``` purescript
newEncryption' :: ServerSideEncryption -> ({ "EncryptionType" :: ServerSideEncryption, "KMSKeyId" :: NullOrUndefined (SSEKMSKeyId), "KMSContext" :: NullOrUndefined (KMSContext) } -> { "EncryptionType" :: ServerSideEncryption, "KMSKeyId" :: NullOrUndefined (SSEKMSKeyId), "KMSContext" :: NullOrUndefined (KMSContext) }) -> Encryption
```

Constructs Encryption's fields from required parameters

#### `EncryptionConfiguration`

``` purescript
newtype EncryptionConfiguration
  = EncryptionConfiguration { "ReplicaKmsKeyID" :: NullOrUndefined (ReplicaKmsKeyID) }
```

Container for information regarding encryption based configuration for replicas.

##### Instances
``` purescript
Newtype EncryptionConfiguration _
Generic EncryptionConfiguration _
Show EncryptionConfiguration
Decode EncryptionConfiguration
Encode EncryptionConfiguration
```

#### `newEncryptionConfiguration`

``` purescript
newEncryptionConfiguration :: EncryptionConfiguration
```

Constructs EncryptionConfiguration from required parameters

#### `newEncryptionConfiguration'`

``` purescript
newEncryptionConfiguration' :: ({ "ReplicaKmsKeyID" :: NullOrUndefined (ReplicaKmsKeyID) } -> { "ReplicaKmsKeyID" :: NullOrUndefined (ReplicaKmsKeyID) }) -> EncryptionConfiguration
```

Constructs EncryptionConfiguration's fields from required parameters

#### `Error`

``` purescript
newtype Error
  = Error { "Key" :: NullOrUndefined (ObjectKey), "VersionId" :: NullOrUndefined (ObjectVersionId), "Code" :: NullOrUndefined (Code), "Message" :: NullOrUndefined (Message) }
```

##### Instances
``` purescript
Newtype Error _
Generic Error _
Show Error
Decode Error
Encode Error
```

#### `newError`

``` purescript
newError :: Error
```

Constructs Error from required parameters

#### `newError'`

``` purescript
newError' :: ({ "Key" :: NullOrUndefined (ObjectKey), "VersionId" :: NullOrUndefined (ObjectVersionId), "Code" :: NullOrUndefined (Code), "Message" :: NullOrUndefined (Message) } -> { "Key" :: NullOrUndefined (ObjectKey), "VersionId" :: NullOrUndefined (ObjectVersionId), "Code" :: NullOrUndefined (Code), "Message" :: NullOrUndefined (Message) }) -> Error
```

Constructs Error's fields from required parameters

#### `ErrorDocument`

``` purescript
newtype ErrorDocument
  = ErrorDocument { "Key" :: ObjectKey }
```

##### Instances
``` purescript
Newtype ErrorDocument _
Generic ErrorDocument _
Show ErrorDocument
Decode ErrorDocument
Encode ErrorDocument
```

#### `newErrorDocument`

``` purescript
newErrorDocument :: ObjectKey -> ErrorDocument
```

Constructs ErrorDocument from required parameters

#### `newErrorDocument'`

``` purescript
newErrorDocument' :: ObjectKey -> ({ "Key" :: ObjectKey } -> { "Key" :: ObjectKey }) -> ErrorDocument
```

Constructs ErrorDocument's fields from required parameters

#### `Errors`

``` purescript
newtype Errors
  = Errors (Array Error)
```

##### Instances
``` purescript
Newtype Errors _
Generic Errors _
Show Errors
Decode Errors
Encode Errors
```

#### `Event`

``` purescript
newtype Event
  = Event String
```

Bucket event for which to send notifications.

##### Instances
``` purescript
Newtype Event _
Generic Event _
Show Event
Decode Event
Encode Event
```

#### `EventList`

``` purescript
newtype EventList
  = EventList (Array Event)
```

##### Instances
``` purescript
Newtype EventList _
Generic EventList _
Show EventList
Decode EventList
Encode EventList
```

#### `Expiration`

``` purescript
newtype Expiration
  = Expiration String
```

##### Instances
``` purescript
Newtype Expiration _
Generic Expiration _
Show Expiration
Decode Expiration
Encode Expiration
```

#### `ExpirationStatus`

``` purescript
newtype ExpirationStatus
  = ExpirationStatus String
```

##### Instances
``` purescript
Newtype ExpirationStatus _
Generic ExpirationStatus _
Show ExpirationStatus
Decode ExpirationStatus
Encode ExpirationStatus
```

#### `ExpiredObjectDeleteMarker`

``` purescript
newtype ExpiredObjectDeleteMarker
  = ExpiredObjectDeleteMarker Boolean
```

##### Instances
``` purescript
Newtype ExpiredObjectDeleteMarker _
Generic ExpiredObjectDeleteMarker _
Show ExpiredObjectDeleteMarker
Decode ExpiredObjectDeleteMarker
Encode ExpiredObjectDeleteMarker
```

#### `Expires`

``` purescript
newtype Expires
  = Expires Timestamp
```

##### Instances
``` purescript
Newtype Expires _
Generic Expires _
Show Expires
Decode Expires
Encode Expires
```

#### `ExposeHeader`

``` purescript
newtype ExposeHeader
  = ExposeHeader String
```

##### Instances
``` purescript
Newtype ExposeHeader _
Generic ExposeHeader _
Show ExposeHeader
Decode ExposeHeader
Encode ExposeHeader
```

#### `ExposeHeaders`

``` purescript
newtype ExposeHeaders
  = ExposeHeaders (Array ExposeHeader)
```

##### Instances
``` purescript
Newtype ExposeHeaders _
Generic ExposeHeaders _
Show ExposeHeaders
Decode ExposeHeaders
Encode ExposeHeaders
```

#### `Expression`

``` purescript
newtype Expression
  = Expression String
```

##### Instances
``` purescript
Newtype Expression _
Generic Expression _
Show Expression
Decode Expression
Encode Expression
```

#### `ExpressionType`

``` purescript
newtype ExpressionType
  = ExpressionType String
```

##### Instances
``` purescript
Newtype ExpressionType _
Generic ExpressionType _
Show ExpressionType
Decode ExpressionType
Encode ExpressionType
```

#### `FetchOwner`

``` purescript
newtype FetchOwner
  = FetchOwner Boolean
```

##### Instances
``` purescript
Newtype FetchOwner _
Generic FetchOwner _
Show FetchOwner
Decode FetchOwner
Encode FetchOwner
```

#### `FieldDelimiter`

``` purescript
newtype FieldDelimiter
  = FieldDelimiter String
```

##### Instances
``` purescript
Newtype FieldDelimiter _
Generic FieldDelimiter _
Show FieldDelimiter
Decode FieldDelimiter
Encode FieldDelimiter
```

#### `FileHeaderInfo`

``` purescript
newtype FileHeaderInfo
  = FileHeaderInfo String
```

##### Instances
``` purescript
Newtype FileHeaderInfo _
Generic FileHeaderInfo _
Show FileHeaderInfo
Decode FileHeaderInfo
Encode FileHeaderInfo
```

#### `FilterRule`

``` purescript
newtype FilterRule
  = FilterRule { "Name" :: NullOrUndefined (FilterRuleName), "Value" :: NullOrUndefined (FilterRuleValue) }
```

Container for key value pair that defines the criteria for the filter rule.

##### Instances
``` purescript
Newtype FilterRule _
Generic FilterRule _
Show FilterRule
Decode FilterRule
Encode FilterRule
```

#### `newFilterRule`

``` purescript
newFilterRule :: FilterRule
```

Constructs FilterRule from required parameters

#### `newFilterRule'`

``` purescript
newFilterRule' :: ({ "Name" :: NullOrUndefined (FilterRuleName), "Value" :: NullOrUndefined (FilterRuleValue) } -> { "Name" :: NullOrUndefined (FilterRuleName), "Value" :: NullOrUndefined (FilterRuleValue) }) -> FilterRule
```

Constructs FilterRule's fields from required parameters

#### `FilterRuleList`

``` purescript
newtype FilterRuleList
  = FilterRuleList (Array FilterRule)
```

A list of containers for key value pair that defines the criteria for the filter rule.

##### Instances
``` purescript
Newtype FilterRuleList _
Generic FilterRuleList _
Show FilterRuleList
Decode FilterRuleList
Encode FilterRuleList
```

#### `FilterRuleName`

``` purescript
newtype FilterRuleName
  = FilterRuleName String
```

##### Instances
``` purescript
Newtype FilterRuleName _
Generic FilterRuleName _
Show FilterRuleName
Decode FilterRuleName
Encode FilterRuleName
```

#### `FilterRuleValue`

``` purescript
newtype FilterRuleValue
  = FilterRuleValue String
```

##### Instances
``` purescript
Newtype FilterRuleValue _
Generic FilterRuleValue _
Show FilterRuleValue
Decode FilterRuleValue
Encode FilterRuleValue
```

#### `GetBucketAccelerateConfigurationOutput`

``` purescript
newtype GetBucketAccelerateConfigurationOutput
  = GetBucketAccelerateConfigurationOutput { "Status" :: NullOrUndefined (BucketAccelerateStatus) }
```

##### Instances
``` purescript
Newtype GetBucketAccelerateConfigurationOutput _
Generic GetBucketAccelerateConfigurationOutput _
Show GetBucketAccelerateConfigurationOutput
Decode GetBucketAccelerateConfigurationOutput
Encode GetBucketAccelerateConfigurationOutput
```

#### `newGetBucketAccelerateConfigurationOutput`

``` purescript
newGetBucketAccelerateConfigurationOutput :: GetBucketAccelerateConfigurationOutput
```

Constructs GetBucketAccelerateConfigurationOutput from required parameters

#### `newGetBucketAccelerateConfigurationOutput'`

``` purescript
newGetBucketAccelerateConfigurationOutput' :: ({ "Status" :: NullOrUndefined (BucketAccelerateStatus) } -> { "Status" :: NullOrUndefined (BucketAccelerateStatus) }) -> GetBucketAccelerateConfigurationOutput
```

Constructs GetBucketAccelerateConfigurationOutput's fields from required parameters

#### `GetBucketAccelerateConfigurationRequest`

``` purescript
newtype GetBucketAccelerateConfigurationRequest
  = GetBucketAccelerateConfigurationRequest { "Bucket" :: BucketName }
```

##### Instances
``` purescript
Newtype GetBucketAccelerateConfigurationRequest _
Generic GetBucketAccelerateConfigurationRequest _
Show GetBucketAccelerateConfigurationRequest
Decode GetBucketAccelerateConfigurationRequest
Encode GetBucketAccelerateConfigurationRequest
```

#### `newGetBucketAccelerateConfigurationRequest`

``` purescript
newGetBucketAccelerateConfigurationRequest :: BucketName -> GetBucketAccelerateConfigurationRequest
```

Constructs GetBucketAccelerateConfigurationRequest from required parameters

#### `newGetBucketAccelerateConfigurationRequest'`

``` purescript
newGetBucketAccelerateConfigurationRequest' :: BucketName -> ({ "Bucket" :: BucketName } -> { "Bucket" :: BucketName }) -> GetBucketAccelerateConfigurationRequest
```

Constructs GetBucketAccelerateConfigurationRequest's fields from required parameters

#### `GetBucketAclOutput`

``` purescript
newtype GetBucketAclOutput
  = GetBucketAclOutput { "Owner" :: NullOrUndefined (Owner), "Grants" :: NullOrUndefined (Grants) }
```

##### Instances
``` purescript
Newtype GetBucketAclOutput _
Generic GetBucketAclOutput _
Show GetBucketAclOutput
Decode GetBucketAclOutput
Encode GetBucketAclOutput
```

#### `newGetBucketAclOutput`

``` purescript
newGetBucketAclOutput :: GetBucketAclOutput
```

Constructs GetBucketAclOutput from required parameters

#### `newGetBucketAclOutput'`

``` purescript
newGetBucketAclOutput' :: ({ "Owner" :: NullOrUndefined (Owner), "Grants" :: NullOrUndefined (Grants) } -> { "Owner" :: NullOrUndefined (Owner), "Grants" :: NullOrUndefined (Grants) }) -> GetBucketAclOutput
```

Constructs GetBucketAclOutput's fields from required parameters

#### `GetBucketAclRequest`

``` purescript
newtype GetBucketAclRequest
  = GetBucketAclRequest { "Bucket" :: BucketName }
```

##### Instances
``` purescript
Newtype GetBucketAclRequest _
Generic GetBucketAclRequest _
Show GetBucketAclRequest
Decode GetBucketAclRequest
Encode GetBucketAclRequest
```

#### `newGetBucketAclRequest`

``` purescript
newGetBucketAclRequest :: BucketName -> GetBucketAclRequest
```

Constructs GetBucketAclRequest from required parameters

#### `newGetBucketAclRequest'`

``` purescript
newGetBucketAclRequest' :: BucketName -> ({ "Bucket" :: BucketName } -> { "Bucket" :: BucketName }) -> GetBucketAclRequest
```

Constructs GetBucketAclRequest's fields from required parameters

#### `GetBucketAnalyticsConfigurationOutput`

``` purescript
newtype GetBucketAnalyticsConfigurationOutput
  = GetBucketAnalyticsConfigurationOutput { "AnalyticsConfiguration" :: NullOrUndefined (AnalyticsConfiguration) }
```

##### Instances
``` purescript
Newtype GetBucketAnalyticsConfigurationOutput _
Generic GetBucketAnalyticsConfigurationOutput _
Show GetBucketAnalyticsConfigurationOutput
Decode GetBucketAnalyticsConfigurationOutput
Encode GetBucketAnalyticsConfigurationOutput
```

#### `newGetBucketAnalyticsConfigurationOutput`

``` purescript
newGetBucketAnalyticsConfigurationOutput :: GetBucketAnalyticsConfigurationOutput
```

Constructs GetBucketAnalyticsConfigurationOutput from required parameters

#### `newGetBucketAnalyticsConfigurationOutput'`

``` purescript
newGetBucketAnalyticsConfigurationOutput' :: ({ "AnalyticsConfiguration" :: NullOrUndefined (AnalyticsConfiguration) } -> { "AnalyticsConfiguration" :: NullOrUndefined (AnalyticsConfiguration) }) -> GetBucketAnalyticsConfigurationOutput
```

Constructs GetBucketAnalyticsConfigurationOutput's fields from required parameters

#### `GetBucketAnalyticsConfigurationRequest`

``` purescript
newtype GetBucketAnalyticsConfigurationRequest
  = GetBucketAnalyticsConfigurationRequest { "Bucket" :: BucketName, "Id" :: AnalyticsId }
```

##### Instances
``` purescript
Newtype GetBucketAnalyticsConfigurationRequest _
Generic GetBucketAnalyticsConfigurationRequest _
Show GetBucketAnalyticsConfigurationRequest
Decode GetBucketAnalyticsConfigurationRequest
Encode GetBucketAnalyticsConfigurationRequest
```

#### `newGetBucketAnalyticsConfigurationRequest`

``` purescript
newGetBucketAnalyticsConfigurationRequest :: BucketName -> AnalyticsId -> GetBucketAnalyticsConfigurationRequest
```

Constructs GetBucketAnalyticsConfigurationRequest from required parameters

#### `newGetBucketAnalyticsConfigurationRequest'`

``` purescript
newGetBucketAnalyticsConfigurationRequest' :: BucketName -> AnalyticsId -> ({ "Bucket" :: BucketName, "Id" :: AnalyticsId } -> { "Bucket" :: BucketName, "Id" :: AnalyticsId }) -> GetBucketAnalyticsConfigurationRequest
```

Constructs GetBucketAnalyticsConfigurationRequest's fields from required parameters

#### `GetBucketCorsOutput`

``` purescript
newtype GetBucketCorsOutput
  = GetBucketCorsOutput { "CORSRules" :: NullOrUndefined (CORSRules) }
```

##### Instances
``` purescript
Newtype GetBucketCorsOutput _
Generic GetBucketCorsOutput _
Show GetBucketCorsOutput
Decode GetBucketCorsOutput
Encode GetBucketCorsOutput
```

#### `newGetBucketCorsOutput`

``` purescript
newGetBucketCorsOutput :: GetBucketCorsOutput
```

Constructs GetBucketCorsOutput from required parameters

#### `newGetBucketCorsOutput'`

``` purescript
newGetBucketCorsOutput' :: ({ "CORSRules" :: NullOrUndefined (CORSRules) } -> { "CORSRules" :: NullOrUndefined (CORSRules) }) -> GetBucketCorsOutput
```

Constructs GetBucketCorsOutput's fields from required parameters

#### `GetBucketCorsRequest`

``` purescript
newtype GetBucketCorsRequest
  = GetBucketCorsRequest { "Bucket" :: BucketName }
```

##### Instances
``` purescript
Newtype GetBucketCorsRequest _
Generic GetBucketCorsRequest _
Show GetBucketCorsRequest
Decode GetBucketCorsRequest
Encode GetBucketCorsRequest
```

#### `newGetBucketCorsRequest`

``` purescript
newGetBucketCorsRequest :: BucketName -> GetBucketCorsRequest
```

Constructs GetBucketCorsRequest from required parameters

#### `newGetBucketCorsRequest'`

``` purescript
newGetBucketCorsRequest' :: BucketName -> ({ "Bucket" :: BucketName } -> { "Bucket" :: BucketName }) -> GetBucketCorsRequest
```

Constructs GetBucketCorsRequest's fields from required parameters

#### `GetBucketEncryptionOutput`

``` purescript
newtype GetBucketEncryptionOutput
  = GetBucketEncryptionOutput { "ServerSideEncryptionConfiguration" :: NullOrUndefined (ServerSideEncryptionConfiguration) }
```

##### Instances
``` purescript
Newtype GetBucketEncryptionOutput _
Generic GetBucketEncryptionOutput _
Show GetBucketEncryptionOutput
Decode GetBucketEncryptionOutput
Encode GetBucketEncryptionOutput
```

#### `newGetBucketEncryptionOutput`

``` purescript
newGetBucketEncryptionOutput :: GetBucketEncryptionOutput
```

Constructs GetBucketEncryptionOutput from required parameters

#### `newGetBucketEncryptionOutput'`

``` purescript
newGetBucketEncryptionOutput' :: ({ "ServerSideEncryptionConfiguration" :: NullOrUndefined (ServerSideEncryptionConfiguration) } -> { "ServerSideEncryptionConfiguration" :: NullOrUndefined (ServerSideEncryptionConfiguration) }) -> GetBucketEncryptionOutput
```

Constructs GetBucketEncryptionOutput's fields from required parameters

#### `GetBucketEncryptionRequest`

``` purescript
newtype GetBucketEncryptionRequest
  = GetBucketEncryptionRequest { "Bucket" :: BucketName }
```

##### Instances
``` purescript
Newtype GetBucketEncryptionRequest _
Generic GetBucketEncryptionRequest _
Show GetBucketEncryptionRequest
Decode GetBucketEncryptionRequest
Encode GetBucketEncryptionRequest
```

#### `newGetBucketEncryptionRequest`

``` purescript
newGetBucketEncryptionRequest :: BucketName -> GetBucketEncryptionRequest
```

Constructs GetBucketEncryptionRequest from required parameters

#### `newGetBucketEncryptionRequest'`

``` purescript
newGetBucketEncryptionRequest' :: BucketName -> ({ "Bucket" :: BucketName } -> { "Bucket" :: BucketName }) -> GetBucketEncryptionRequest
```

Constructs GetBucketEncryptionRequest's fields from required parameters

#### `GetBucketInventoryConfigurationOutput`

``` purescript
newtype GetBucketInventoryConfigurationOutput
  = GetBucketInventoryConfigurationOutput { "InventoryConfiguration" :: NullOrUndefined (InventoryConfiguration) }
```

##### Instances
``` purescript
Newtype GetBucketInventoryConfigurationOutput _
Generic GetBucketInventoryConfigurationOutput _
Show GetBucketInventoryConfigurationOutput
Decode GetBucketInventoryConfigurationOutput
Encode GetBucketInventoryConfigurationOutput
```

#### `newGetBucketInventoryConfigurationOutput`

``` purescript
newGetBucketInventoryConfigurationOutput :: GetBucketInventoryConfigurationOutput
```

Constructs GetBucketInventoryConfigurationOutput from required parameters

#### `newGetBucketInventoryConfigurationOutput'`

``` purescript
newGetBucketInventoryConfigurationOutput' :: ({ "InventoryConfiguration" :: NullOrUndefined (InventoryConfiguration) } -> { "InventoryConfiguration" :: NullOrUndefined (InventoryConfiguration) }) -> GetBucketInventoryConfigurationOutput
```

Constructs GetBucketInventoryConfigurationOutput's fields from required parameters

#### `GetBucketInventoryConfigurationRequest`

``` purescript
newtype GetBucketInventoryConfigurationRequest
  = GetBucketInventoryConfigurationRequest { "Bucket" :: BucketName, "Id" :: InventoryId }
```

##### Instances
``` purescript
Newtype GetBucketInventoryConfigurationRequest _
Generic GetBucketInventoryConfigurationRequest _
Show GetBucketInventoryConfigurationRequest
Decode GetBucketInventoryConfigurationRequest
Encode GetBucketInventoryConfigurationRequest
```

#### `newGetBucketInventoryConfigurationRequest`

``` purescript
newGetBucketInventoryConfigurationRequest :: BucketName -> InventoryId -> GetBucketInventoryConfigurationRequest
```

Constructs GetBucketInventoryConfigurationRequest from required parameters

#### `newGetBucketInventoryConfigurationRequest'`

``` purescript
newGetBucketInventoryConfigurationRequest' :: BucketName -> InventoryId -> ({ "Bucket" :: BucketName, "Id" :: InventoryId } -> { "Bucket" :: BucketName, "Id" :: InventoryId }) -> GetBucketInventoryConfigurationRequest
```

Constructs GetBucketInventoryConfigurationRequest's fields from required parameters

#### `GetBucketLifecycleConfigurationOutput`

``` purescript
newtype GetBucketLifecycleConfigurationOutput
  = GetBucketLifecycleConfigurationOutput { "Rules" :: NullOrUndefined (LifecycleRules) }
```

##### Instances
``` purescript
Newtype GetBucketLifecycleConfigurationOutput _
Generic GetBucketLifecycleConfigurationOutput _
Show GetBucketLifecycleConfigurationOutput
Decode GetBucketLifecycleConfigurationOutput
Encode GetBucketLifecycleConfigurationOutput
```

#### `newGetBucketLifecycleConfigurationOutput`

``` purescript
newGetBucketLifecycleConfigurationOutput :: GetBucketLifecycleConfigurationOutput
```

Constructs GetBucketLifecycleConfigurationOutput from required parameters

#### `newGetBucketLifecycleConfigurationOutput'`

``` purescript
newGetBucketLifecycleConfigurationOutput' :: ({ "Rules" :: NullOrUndefined (LifecycleRules) } -> { "Rules" :: NullOrUndefined (LifecycleRules) }) -> GetBucketLifecycleConfigurationOutput
```

Constructs GetBucketLifecycleConfigurationOutput's fields from required parameters

#### `GetBucketLifecycleConfigurationRequest`

``` purescript
newtype GetBucketLifecycleConfigurationRequest
  = GetBucketLifecycleConfigurationRequest { "Bucket" :: BucketName }
```

##### Instances
``` purescript
Newtype GetBucketLifecycleConfigurationRequest _
Generic GetBucketLifecycleConfigurationRequest _
Show GetBucketLifecycleConfigurationRequest
Decode GetBucketLifecycleConfigurationRequest
Encode GetBucketLifecycleConfigurationRequest
```

#### `newGetBucketLifecycleConfigurationRequest`

``` purescript
newGetBucketLifecycleConfigurationRequest :: BucketName -> GetBucketLifecycleConfigurationRequest
```

Constructs GetBucketLifecycleConfigurationRequest from required parameters

#### `newGetBucketLifecycleConfigurationRequest'`

``` purescript
newGetBucketLifecycleConfigurationRequest' :: BucketName -> ({ "Bucket" :: BucketName } -> { "Bucket" :: BucketName }) -> GetBucketLifecycleConfigurationRequest
```

Constructs GetBucketLifecycleConfigurationRequest's fields from required parameters

#### `GetBucketLifecycleOutput`

``` purescript
newtype GetBucketLifecycleOutput
  = GetBucketLifecycleOutput { "Rules" :: NullOrUndefined (Rules) }
```

##### Instances
``` purescript
Newtype GetBucketLifecycleOutput _
Generic GetBucketLifecycleOutput _
Show GetBucketLifecycleOutput
Decode GetBucketLifecycleOutput
Encode GetBucketLifecycleOutput
```

#### `newGetBucketLifecycleOutput`

``` purescript
newGetBucketLifecycleOutput :: GetBucketLifecycleOutput
```

Constructs GetBucketLifecycleOutput from required parameters

#### `newGetBucketLifecycleOutput'`

``` purescript
newGetBucketLifecycleOutput' :: ({ "Rules" :: NullOrUndefined (Rules) } -> { "Rules" :: NullOrUndefined (Rules) }) -> GetBucketLifecycleOutput
```

Constructs GetBucketLifecycleOutput's fields from required parameters

#### `GetBucketLifecycleRequest`

``` purescript
newtype GetBucketLifecycleRequest
  = GetBucketLifecycleRequest { "Bucket" :: BucketName }
```

##### Instances
``` purescript
Newtype GetBucketLifecycleRequest _
Generic GetBucketLifecycleRequest _
Show GetBucketLifecycleRequest
Decode GetBucketLifecycleRequest
Encode GetBucketLifecycleRequest
```

#### `newGetBucketLifecycleRequest`

``` purescript
newGetBucketLifecycleRequest :: BucketName -> GetBucketLifecycleRequest
```

Constructs GetBucketLifecycleRequest from required parameters

#### `newGetBucketLifecycleRequest'`

``` purescript
newGetBucketLifecycleRequest' :: BucketName -> ({ "Bucket" :: BucketName } -> { "Bucket" :: BucketName }) -> GetBucketLifecycleRequest
```

Constructs GetBucketLifecycleRequest's fields from required parameters

#### `GetBucketLocationOutput`

``` purescript
newtype GetBucketLocationOutput
  = GetBucketLocationOutput { "LocationConstraint" :: NullOrUndefined (BucketLocationConstraint) }
```

##### Instances
``` purescript
Newtype GetBucketLocationOutput _
Generic GetBucketLocationOutput _
Show GetBucketLocationOutput
Decode GetBucketLocationOutput
Encode GetBucketLocationOutput
```

#### `newGetBucketLocationOutput`

``` purescript
newGetBucketLocationOutput :: GetBucketLocationOutput
```

Constructs GetBucketLocationOutput from required parameters

#### `newGetBucketLocationOutput'`

``` purescript
newGetBucketLocationOutput' :: ({ "LocationConstraint" :: NullOrUndefined (BucketLocationConstraint) } -> { "LocationConstraint" :: NullOrUndefined (BucketLocationConstraint) }) -> GetBucketLocationOutput
```

Constructs GetBucketLocationOutput's fields from required parameters

#### `GetBucketLocationRequest`

``` purescript
newtype GetBucketLocationRequest
  = GetBucketLocationRequest { "Bucket" :: BucketName }
```

##### Instances
``` purescript
Newtype GetBucketLocationRequest _
Generic GetBucketLocationRequest _
Show GetBucketLocationRequest
Decode GetBucketLocationRequest
Encode GetBucketLocationRequest
```

#### `newGetBucketLocationRequest`

``` purescript
newGetBucketLocationRequest :: BucketName -> GetBucketLocationRequest
```

Constructs GetBucketLocationRequest from required parameters

#### `newGetBucketLocationRequest'`

``` purescript
newGetBucketLocationRequest' :: BucketName -> ({ "Bucket" :: BucketName } -> { "Bucket" :: BucketName }) -> GetBucketLocationRequest
```

Constructs GetBucketLocationRequest's fields from required parameters

#### `GetBucketLoggingOutput`

``` purescript
newtype GetBucketLoggingOutput
  = GetBucketLoggingOutput { "LoggingEnabled" :: NullOrUndefined (LoggingEnabled) }
```

##### Instances
``` purescript
Newtype GetBucketLoggingOutput _
Generic GetBucketLoggingOutput _
Show GetBucketLoggingOutput
Decode GetBucketLoggingOutput
Encode GetBucketLoggingOutput
```

#### `newGetBucketLoggingOutput`

``` purescript
newGetBucketLoggingOutput :: GetBucketLoggingOutput
```

Constructs GetBucketLoggingOutput from required parameters

#### `newGetBucketLoggingOutput'`

``` purescript
newGetBucketLoggingOutput' :: ({ "LoggingEnabled" :: NullOrUndefined (LoggingEnabled) } -> { "LoggingEnabled" :: NullOrUndefined (LoggingEnabled) }) -> GetBucketLoggingOutput
```

Constructs GetBucketLoggingOutput's fields from required parameters

#### `GetBucketLoggingRequest`

``` purescript
newtype GetBucketLoggingRequest
  = GetBucketLoggingRequest { "Bucket" :: BucketName }
```

##### Instances
``` purescript
Newtype GetBucketLoggingRequest _
Generic GetBucketLoggingRequest _
Show GetBucketLoggingRequest
Decode GetBucketLoggingRequest
Encode GetBucketLoggingRequest
```

#### `newGetBucketLoggingRequest`

``` purescript
newGetBucketLoggingRequest :: BucketName -> GetBucketLoggingRequest
```

Constructs GetBucketLoggingRequest from required parameters

#### `newGetBucketLoggingRequest'`

``` purescript
newGetBucketLoggingRequest' :: BucketName -> ({ "Bucket" :: BucketName } -> { "Bucket" :: BucketName }) -> GetBucketLoggingRequest
```

Constructs GetBucketLoggingRequest's fields from required parameters

#### `GetBucketMetricsConfigurationOutput`

``` purescript
newtype GetBucketMetricsConfigurationOutput
  = GetBucketMetricsConfigurationOutput { "MetricsConfiguration" :: NullOrUndefined (MetricsConfiguration) }
```

##### Instances
``` purescript
Newtype GetBucketMetricsConfigurationOutput _
Generic GetBucketMetricsConfigurationOutput _
Show GetBucketMetricsConfigurationOutput
Decode GetBucketMetricsConfigurationOutput
Encode GetBucketMetricsConfigurationOutput
```

#### `newGetBucketMetricsConfigurationOutput`

``` purescript
newGetBucketMetricsConfigurationOutput :: GetBucketMetricsConfigurationOutput
```

Constructs GetBucketMetricsConfigurationOutput from required parameters

#### `newGetBucketMetricsConfigurationOutput'`

``` purescript
newGetBucketMetricsConfigurationOutput' :: ({ "MetricsConfiguration" :: NullOrUndefined (MetricsConfiguration) } -> { "MetricsConfiguration" :: NullOrUndefined (MetricsConfiguration) }) -> GetBucketMetricsConfigurationOutput
```

Constructs GetBucketMetricsConfigurationOutput's fields from required parameters

#### `GetBucketMetricsConfigurationRequest`

``` purescript
newtype GetBucketMetricsConfigurationRequest
  = GetBucketMetricsConfigurationRequest { "Bucket" :: BucketName, "Id" :: MetricsId }
```

##### Instances
``` purescript
Newtype GetBucketMetricsConfigurationRequest _
Generic GetBucketMetricsConfigurationRequest _
Show GetBucketMetricsConfigurationRequest
Decode GetBucketMetricsConfigurationRequest
Encode GetBucketMetricsConfigurationRequest
```

#### `newGetBucketMetricsConfigurationRequest`

``` purescript
newGetBucketMetricsConfigurationRequest :: BucketName -> MetricsId -> GetBucketMetricsConfigurationRequest
```

Constructs GetBucketMetricsConfigurationRequest from required parameters

#### `newGetBucketMetricsConfigurationRequest'`

``` purescript
newGetBucketMetricsConfigurationRequest' :: BucketName -> MetricsId -> ({ "Bucket" :: BucketName, "Id" :: MetricsId } -> { "Bucket" :: BucketName, "Id" :: MetricsId }) -> GetBucketMetricsConfigurationRequest
```

Constructs GetBucketMetricsConfigurationRequest's fields from required parameters

#### `GetBucketNotificationConfigurationRequest`

``` purescript
newtype GetBucketNotificationConfigurationRequest
  = GetBucketNotificationConfigurationRequest { "Bucket" :: BucketName }
```

##### Instances
``` purescript
Newtype GetBucketNotificationConfigurationRequest _
Generic GetBucketNotificationConfigurationRequest _
Show GetBucketNotificationConfigurationRequest
Decode GetBucketNotificationConfigurationRequest
Encode GetBucketNotificationConfigurationRequest
```

#### `newGetBucketNotificationConfigurationRequest`

``` purescript
newGetBucketNotificationConfigurationRequest :: BucketName -> GetBucketNotificationConfigurationRequest
```

Constructs GetBucketNotificationConfigurationRequest from required parameters

#### `newGetBucketNotificationConfigurationRequest'`

``` purescript
newGetBucketNotificationConfigurationRequest' :: BucketName -> ({ "Bucket" :: BucketName } -> { "Bucket" :: BucketName }) -> GetBucketNotificationConfigurationRequest
```

Constructs GetBucketNotificationConfigurationRequest's fields from required parameters

#### `GetBucketPolicyOutput`

``` purescript
newtype GetBucketPolicyOutput
  = GetBucketPolicyOutput { "Policy" :: NullOrUndefined (Policy) }
```

##### Instances
``` purescript
Newtype GetBucketPolicyOutput _
Generic GetBucketPolicyOutput _
Show GetBucketPolicyOutput
Decode GetBucketPolicyOutput
Encode GetBucketPolicyOutput
```

#### `newGetBucketPolicyOutput`

``` purescript
newGetBucketPolicyOutput :: GetBucketPolicyOutput
```

Constructs GetBucketPolicyOutput from required parameters

#### `newGetBucketPolicyOutput'`

``` purescript
newGetBucketPolicyOutput' :: ({ "Policy" :: NullOrUndefined (Policy) } -> { "Policy" :: NullOrUndefined (Policy) }) -> GetBucketPolicyOutput
```

Constructs GetBucketPolicyOutput's fields from required parameters

#### `GetBucketPolicyRequest`

``` purescript
newtype GetBucketPolicyRequest
  = GetBucketPolicyRequest { "Bucket" :: BucketName }
```

##### Instances
``` purescript
Newtype GetBucketPolicyRequest _
Generic GetBucketPolicyRequest _
Show GetBucketPolicyRequest
Decode GetBucketPolicyRequest
Encode GetBucketPolicyRequest
```

#### `newGetBucketPolicyRequest`

``` purescript
newGetBucketPolicyRequest :: BucketName -> GetBucketPolicyRequest
```

Constructs GetBucketPolicyRequest from required parameters

#### `newGetBucketPolicyRequest'`

``` purescript
newGetBucketPolicyRequest' :: BucketName -> ({ "Bucket" :: BucketName } -> { "Bucket" :: BucketName }) -> GetBucketPolicyRequest
```

Constructs GetBucketPolicyRequest's fields from required parameters

#### `GetBucketReplicationOutput`

``` purescript
newtype GetBucketReplicationOutput
  = GetBucketReplicationOutput { "ReplicationConfiguration" :: NullOrUndefined (ReplicationConfiguration) }
```

##### Instances
``` purescript
Newtype GetBucketReplicationOutput _
Generic GetBucketReplicationOutput _
Show GetBucketReplicationOutput
Decode GetBucketReplicationOutput
Encode GetBucketReplicationOutput
```

#### `newGetBucketReplicationOutput`

``` purescript
newGetBucketReplicationOutput :: GetBucketReplicationOutput
```

Constructs GetBucketReplicationOutput from required parameters

#### `newGetBucketReplicationOutput'`

``` purescript
newGetBucketReplicationOutput' :: ({ "ReplicationConfiguration" :: NullOrUndefined (ReplicationConfiguration) } -> { "ReplicationConfiguration" :: NullOrUndefined (ReplicationConfiguration) }) -> GetBucketReplicationOutput
```

Constructs GetBucketReplicationOutput's fields from required parameters

#### `GetBucketReplicationRequest`

``` purescript
newtype GetBucketReplicationRequest
  = GetBucketReplicationRequest { "Bucket" :: BucketName }
```

##### Instances
``` purescript
Newtype GetBucketReplicationRequest _
Generic GetBucketReplicationRequest _
Show GetBucketReplicationRequest
Decode GetBucketReplicationRequest
Encode GetBucketReplicationRequest
```

#### `newGetBucketReplicationRequest`

``` purescript
newGetBucketReplicationRequest :: BucketName -> GetBucketReplicationRequest
```

Constructs GetBucketReplicationRequest from required parameters

#### `newGetBucketReplicationRequest'`

``` purescript
newGetBucketReplicationRequest' :: BucketName -> ({ "Bucket" :: BucketName } -> { "Bucket" :: BucketName }) -> GetBucketReplicationRequest
```

Constructs GetBucketReplicationRequest's fields from required parameters

#### `GetBucketRequestPaymentOutput`

``` purescript
newtype GetBucketRequestPaymentOutput
  = GetBucketRequestPaymentOutput { "Payer" :: NullOrUndefined (Payer) }
```

##### Instances
``` purescript
Newtype GetBucketRequestPaymentOutput _
Generic GetBucketRequestPaymentOutput _
Show GetBucketRequestPaymentOutput
Decode GetBucketRequestPaymentOutput
Encode GetBucketRequestPaymentOutput
```

#### `newGetBucketRequestPaymentOutput`

``` purescript
newGetBucketRequestPaymentOutput :: GetBucketRequestPaymentOutput
```

Constructs GetBucketRequestPaymentOutput from required parameters

#### `newGetBucketRequestPaymentOutput'`

``` purescript
newGetBucketRequestPaymentOutput' :: ({ "Payer" :: NullOrUndefined (Payer) } -> { "Payer" :: NullOrUndefined (Payer) }) -> GetBucketRequestPaymentOutput
```

Constructs GetBucketRequestPaymentOutput's fields from required parameters

#### `GetBucketRequestPaymentRequest`

``` purescript
newtype GetBucketRequestPaymentRequest
  = GetBucketRequestPaymentRequest { "Bucket" :: BucketName }
```

##### Instances
``` purescript
Newtype GetBucketRequestPaymentRequest _
Generic GetBucketRequestPaymentRequest _
Show GetBucketRequestPaymentRequest
Decode GetBucketRequestPaymentRequest
Encode GetBucketRequestPaymentRequest
```

#### `newGetBucketRequestPaymentRequest`

``` purescript
newGetBucketRequestPaymentRequest :: BucketName -> GetBucketRequestPaymentRequest
```

Constructs GetBucketRequestPaymentRequest from required parameters

#### `newGetBucketRequestPaymentRequest'`

``` purescript
newGetBucketRequestPaymentRequest' :: BucketName -> ({ "Bucket" :: BucketName } -> { "Bucket" :: BucketName }) -> GetBucketRequestPaymentRequest
```

Constructs GetBucketRequestPaymentRequest's fields from required parameters

#### `GetBucketTaggingOutput`

``` purescript
newtype GetBucketTaggingOutput
  = GetBucketTaggingOutput { "TagSet" :: TagSet }
```

##### Instances
``` purescript
Newtype GetBucketTaggingOutput _
Generic GetBucketTaggingOutput _
Show GetBucketTaggingOutput
Decode GetBucketTaggingOutput
Encode GetBucketTaggingOutput
```

#### `newGetBucketTaggingOutput`

``` purescript
newGetBucketTaggingOutput :: TagSet -> GetBucketTaggingOutput
```

Constructs GetBucketTaggingOutput from required parameters

#### `newGetBucketTaggingOutput'`

``` purescript
newGetBucketTaggingOutput' :: TagSet -> ({ "TagSet" :: TagSet } -> { "TagSet" :: TagSet }) -> GetBucketTaggingOutput
```

Constructs GetBucketTaggingOutput's fields from required parameters

#### `GetBucketTaggingRequest`

``` purescript
newtype GetBucketTaggingRequest
  = GetBucketTaggingRequest { "Bucket" :: BucketName }
```

##### Instances
``` purescript
Newtype GetBucketTaggingRequest _
Generic GetBucketTaggingRequest _
Show GetBucketTaggingRequest
Decode GetBucketTaggingRequest
Encode GetBucketTaggingRequest
```

#### `newGetBucketTaggingRequest`

``` purescript
newGetBucketTaggingRequest :: BucketName -> GetBucketTaggingRequest
```

Constructs GetBucketTaggingRequest from required parameters

#### `newGetBucketTaggingRequest'`

``` purescript
newGetBucketTaggingRequest' :: BucketName -> ({ "Bucket" :: BucketName } -> { "Bucket" :: BucketName }) -> GetBucketTaggingRequest
```

Constructs GetBucketTaggingRequest's fields from required parameters

#### `GetBucketVersioningOutput`

``` purescript
newtype GetBucketVersioningOutput
  = GetBucketVersioningOutput { "Status" :: NullOrUndefined (BucketVersioningStatus), "MFADelete" :: NullOrUndefined (MFADeleteStatus) }
```

##### Instances
``` purescript
Newtype GetBucketVersioningOutput _
Generic GetBucketVersioningOutput _
Show GetBucketVersioningOutput
Decode GetBucketVersioningOutput
Encode GetBucketVersioningOutput
```

#### `newGetBucketVersioningOutput`

``` purescript
newGetBucketVersioningOutput :: GetBucketVersioningOutput
```

Constructs GetBucketVersioningOutput from required parameters

#### `newGetBucketVersioningOutput'`

``` purescript
newGetBucketVersioningOutput' :: ({ "Status" :: NullOrUndefined (BucketVersioningStatus), "MFADelete" :: NullOrUndefined (MFADeleteStatus) } -> { "Status" :: NullOrUndefined (BucketVersioningStatus), "MFADelete" :: NullOrUndefined (MFADeleteStatus) }) -> GetBucketVersioningOutput
```

Constructs GetBucketVersioningOutput's fields from required parameters

#### `GetBucketVersioningRequest`

``` purescript
newtype GetBucketVersioningRequest
  = GetBucketVersioningRequest { "Bucket" :: BucketName }
```

##### Instances
``` purescript
Newtype GetBucketVersioningRequest _
Generic GetBucketVersioningRequest _
Show GetBucketVersioningRequest
Decode GetBucketVersioningRequest
Encode GetBucketVersioningRequest
```

#### `newGetBucketVersioningRequest`

``` purescript
newGetBucketVersioningRequest :: BucketName -> GetBucketVersioningRequest
```

Constructs GetBucketVersioningRequest from required parameters

#### `newGetBucketVersioningRequest'`

``` purescript
newGetBucketVersioningRequest' :: BucketName -> ({ "Bucket" :: BucketName } -> { "Bucket" :: BucketName }) -> GetBucketVersioningRequest
```

Constructs GetBucketVersioningRequest's fields from required parameters

#### `GetBucketWebsiteOutput`

``` purescript
newtype GetBucketWebsiteOutput
  = GetBucketWebsiteOutput { "RedirectAllRequestsTo" :: NullOrUndefined (RedirectAllRequestsTo), "IndexDocument" :: NullOrUndefined (IndexDocument), "ErrorDocument" :: NullOrUndefined (ErrorDocument), "RoutingRules" :: NullOrUndefined (RoutingRules) }
```

##### Instances
``` purescript
Newtype GetBucketWebsiteOutput _
Generic GetBucketWebsiteOutput _
Show GetBucketWebsiteOutput
Decode GetBucketWebsiteOutput
Encode GetBucketWebsiteOutput
```

#### `newGetBucketWebsiteOutput`

``` purescript
newGetBucketWebsiteOutput :: GetBucketWebsiteOutput
```

Constructs GetBucketWebsiteOutput from required parameters

#### `newGetBucketWebsiteOutput'`

``` purescript
newGetBucketWebsiteOutput' :: ({ "RedirectAllRequestsTo" :: NullOrUndefined (RedirectAllRequestsTo), "IndexDocument" :: NullOrUndefined (IndexDocument), "ErrorDocument" :: NullOrUndefined (ErrorDocument), "RoutingRules" :: NullOrUndefined (RoutingRules) } -> { "RedirectAllRequestsTo" :: NullOrUndefined (RedirectAllRequestsTo), "IndexDocument" :: NullOrUndefined (IndexDocument), "ErrorDocument" :: NullOrUndefined (ErrorDocument), "RoutingRules" :: NullOrUndefined (RoutingRules) }) -> GetBucketWebsiteOutput
```

Constructs GetBucketWebsiteOutput's fields from required parameters

#### `GetBucketWebsiteRequest`

``` purescript
newtype GetBucketWebsiteRequest
  = GetBucketWebsiteRequest { "Bucket" :: BucketName }
```

##### Instances
``` purescript
Newtype GetBucketWebsiteRequest _
Generic GetBucketWebsiteRequest _
Show GetBucketWebsiteRequest
Decode GetBucketWebsiteRequest
Encode GetBucketWebsiteRequest
```

#### `newGetBucketWebsiteRequest`

``` purescript
newGetBucketWebsiteRequest :: BucketName -> GetBucketWebsiteRequest
```

Constructs GetBucketWebsiteRequest from required parameters

#### `newGetBucketWebsiteRequest'`

``` purescript
newGetBucketWebsiteRequest' :: BucketName -> ({ "Bucket" :: BucketName } -> { "Bucket" :: BucketName }) -> GetBucketWebsiteRequest
```

Constructs GetBucketWebsiteRequest's fields from required parameters

#### `GetObjectAclOutput`

``` purescript
newtype GetObjectAclOutput
  = GetObjectAclOutput { "Owner" :: NullOrUndefined (Owner), "Grants" :: NullOrUndefined (Grants), "RequestCharged" :: NullOrUndefined (RequestCharged) }
```

##### Instances
``` purescript
Newtype GetObjectAclOutput _
Generic GetObjectAclOutput _
Show GetObjectAclOutput
Decode GetObjectAclOutput
Encode GetObjectAclOutput
```

#### `newGetObjectAclOutput`

``` purescript
newGetObjectAclOutput :: GetObjectAclOutput
```

Constructs GetObjectAclOutput from required parameters

#### `newGetObjectAclOutput'`

``` purescript
newGetObjectAclOutput' :: ({ "Owner" :: NullOrUndefined (Owner), "Grants" :: NullOrUndefined (Grants), "RequestCharged" :: NullOrUndefined (RequestCharged) } -> { "Owner" :: NullOrUndefined (Owner), "Grants" :: NullOrUndefined (Grants), "RequestCharged" :: NullOrUndefined (RequestCharged) }) -> GetObjectAclOutput
```

Constructs GetObjectAclOutput's fields from required parameters

#### `GetObjectAclRequest`

``` purescript
newtype GetObjectAclRequest
  = GetObjectAclRequest { "Bucket" :: BucketName, "Key" :: ObjectKey, "VersionId" :: NullOrUndefined (ObjectVersionId), "RequestPayer" :: NullOrUndefined (RequestPayer) }
```

##### Instances
``` purescript
Newtype GetObjectAclRequest _
Generic GetObjectAclRequest _
Show GetObjectAclRequest
Decode GetObjectAclRequest
Encode GetObjectAclRequest
```

#### `newGetObjectAclRequest`

``` purescript
newGetObjectAclRequest :: BucketName -> ObjectKey -> GetObjectAclRequest
```

Constructs GetObjectAclRequest from required parameters

#### `newGetObjectAclRequest'`

``` purescript
newGetObjectAclRequest' :: BucketName -> ObjectKey -> ({ "Bucket" :: BucketName, "Key" :: ObjectKey, "VersionId" :: NullOrUndefined (ObjectVersionId), "RequestPayer" :: NullOrUndefined (RequestPayer) } -> { "Bucket" :: BucketName, "Key" :: ObjectKey, "VersionId" :: NullOrUndefined (ObjectVersionId), "RequestPayer" :: NullOrUndefined (RequestPayer) }) -> GetObjectAclRequest
```

Constructs GetObjectAclRequest's fields from required parameters

#### `GetObjectOutput`

``` purescript
newtype GetObjectOutput
  = GetObjectOutput { "Body" :: NullOrUndefined (Body), "DeleteMarker" :: NullOrUndefined (DeleteMarker), "AcceptRanges" :: NullOrUndefined (AcceptRanges), "Expiration" :: NullOrUndefined (Expiration), "Restore" :: NullOrUndefined (Restore), "LastModified" :: NullOrUndefined (LastModified), "ContentLength" :: NullOrUndefined (ContentLength), "ETag" :: NullOrUndefined (ETag), "MissingMeta" :: NullOrUndefined (MissingMeta), "VersionId" :: NullOrUndefined (ObjectVersionId), "CacheControl" :: NullOrUndefined (CacheControl), "ContentDisposition" :: NullOrUndefined (ContentDisposition), "ContentEncoding" :: NullOrUndefined (ContentEncoding), "ContentLanguage" :: NullOrUndefined (ContentLanguage), "ContentRange" :: NullOrUndefined (ContentRange), "ContentType" :: NullOrUndefined (ContentType), "Expires" :: NullOrUndefined (Expires), "WebsiteRedirectLocation" :: NullOrUndefined (WebsiteRedirectLocation), "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption), "Metadata" :: NullOrUndefined (Metadata), "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm), "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5), "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId), "StorageClass" :: NullOrUndefined (StorageClass), "RequestCharged" :: NullOrUndefined (RequestCharged), "ReplicationStatus" :: NullOrUndefined (ReplicationStatus), "PartsCount" :: NullOrUndefined (PartsCount), "TagCount" :: NullOrUndefined (TagCount) }
```

##### Instances
``` purescript
Newtype GetObjectOutput _
Generic GetObjectOutput _
Show GetObjectOutput
Decode GetObjectOutput
Encode GetObjectOutput
```

#### `newGetObjectOutput`

``` purescript
newGetObjectOutput :: GetObjectOutput
```

Constructs GetObjectOutput from required parameters

#### `newGetObjectOutput'`

``` purescript
newGetObjectOutput' :: ({ "Body" :: NullOrUndefined (Body), "DeleteMarker" :: NullOrUndefined (DeleteMarker), "AcceptRanges" :: NullOrUndefined (AcceptRanges), "Expiration" :: NullOrUndefined (Expiration), "Restore" :: NullOrUndefined (Restore), "LastModified" :: NullOrUndefined (LastModified), "ContentLength" :: NullOrUndefined (ContentLength), "ETag" :: NullOrUndefined (ETag), "MissingMeta" :: NullOrUndefined (MissingMeta), "VersionId" :: NullOrUndefined (ObjectVersionId), "CacheControl" :: NullOrUndefined (CacheControl), "ContentDisposition" :: NullOrUndefined (ContentDisposition), "ContentEncoding" :: NullOrUndefined (ContentEncoding), "ContentLanguage" :: NullOrUndefined (ContentLanguage), "ContentRange" :: NullOrUndefined (ContentRange), "ContentType" :: NullOrUndefined (ContentType), "Expires" :: NullOrUndefined (Expires), "WebsiteRedirectLocation" :: NullOrUndefined (WebsiteRedirectLocation), "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption), "Metadata" :: NullOrUndefined (Metadata), "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm), "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5), "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId), "StorageClass" :: NullOrUndefined (StorageClass), "RequestCharged" :: NullOrUndefined (RequestCharged), "ReplicationStatus" :: NullOrUndefined (ReplicationStatus), "PartsCount" :: NullOrUndefined (PartsCount), "TagCount" :: NullOrUndefined (TagCount) } -> { "Body" :: NullOrUndefined (Body), "DeleteMarker" :: NullOrUndefined (DeleteMarker), "AcceptRanges" :: NullOrUndefined (AcceptRanges), "Expiration" :: NullOrUndefined (Expiration), "Restore" :: NullOrUndefined (Restore), "LastModified" :: NullOrUndefined (LastModified), "ContentLength" :: NullOrUndefined (ContentLength), "ETag" :: NullOrUndefined (ETag), "MissingMeta" :: NullOrUndefined (MissingMeta), "VersionId" :: NullOrUndefined (ObjectVersionId), "CacheControl" :: NullOrUndefined (CacheControl), "ContentDisposition" :: NullOrUndefined (ContentDisposition), "ContentEncoding" :: NullOrUndefined (ContentEncoding), "ContentLanguage" :: NullOrUndefined (ContentLanguage), "ContentRange" :: NullOrUndefined (ContentRange), "ContentType" :: NullOrUndefined (ContentType), "Expires" :: NullOrUndefined (Expires), "WebsiteRedirectLocation" :: NullOrUndefined (WebsiteRedirectLocation), "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption), "Metadata" :: NullOrUndefined (Metadata), "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm), "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5), "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId), "StorageClass" :: NullOrUndefined (StorageClass), "RequestCharged" :: NullOrUndefined (RequestCharged), "ReplicationStatus" :: NullOrUndefined (ReplicationStatus), "PartsCount" :: NullOrUndefined (PartsCount), "TagCount" :: NullOrUndefined (TagCount) }) -> GetObjectOutput
```

Constructs GetObjectOutput's fields from required parameters

#### `GetObjectRequest`

``` purescript
newtype GetObjectRequest
  = GetObjectRequest { "Bucket" :: BucketName, "IfMatch" :: NullOrUndefined (IfMatch), "IfModifiedSince" :: NullOrUndefined (IfModifiedSince), "IfNoneMatch" :: NullOrUndefined (IfNoneMatch), "IfUnmodifiedSince" :: NullOrUndefined (IfUnmodifiedSince), "Key" :: ObjectKey, "Range" :: NullOrUndefined (Range), "ResponseCacheControl" :: NullOrUndefined (ResponseCacheControl), "ResponseContentDisposition" :: NullOrUndefined (ResponseContentDisposition), "ResponseContentEncoding" :: NullOrUndefined (ResponseContentEncoding), "ResponseContentLanguage" :: NullOrUndefined (ResponseContentLanguage), "ResponseContentType" :: NullOrUndefined (ResponseContentType), "ResponseExpires" :: NullOrUndefined (ResponseExpires), "VersionId" :: NullOrUndefined (ObjectVersionId), "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm), "SSECustomerKey" :: NullOrUndefined (SSECustomerKey), "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5), "RequestPayer" :: NullOrUndefined (RequestPayer), "PartNumber" :: NullOrUndefined (PartNumber) }
```

##### Instances
``` purescript
Newtype GetObjectRequest _
Generic GetObjectRequest _
Show GetObjectRequest
Decode GetObjectRequest
Encode GetObjectRequest
```

#### `newGetObjectRequest`

``` purescript
newGetObjectRequest :: BucketName -> ObjectKey -> GetObjectRequest
```

Constructs GetObjectRequest from required parameters

#### `newGetObjectRequest'`

``` purescript
newGetObjectRequest' :: BucketName -> ObjectKey -> ({ "Bucket" :: BucketName, "IfMatch" :: NullOrUndefined (IfMatch), "IfModifiedSince" :: NullOrUndefined (IfModifiedSince), "IfNoneMatch" :: NullOrUndefined (IfNoneMatch), "IfUnmodifiedSince" :: NullOrUndefined (IfUnmodifiedSince), "Key" :: ObjectKey, "Range" :: NullOrUndefined (Range), "ResponseCacheControl" :: NullOrUndefined (ResponseCacheControl), "ResponseContentDisposition" :: NullOrUndefined (ResponseContentDisposition), "ResponseContentEncoding" :: NullOrUndefined (ResponseContentEncoding), "ResponseContentLanguage" :: NullOrUndefined (ResponseContentLanguage), "ResponseContentType" :: NullOrUndefined (ResponseContentType), "ResponseExpires" :: NullOrUndefined (ResponseExpires), "VersionId" :: NullOrUndefined (ObjectVersionId), "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm), "SSECustomerKey" :: NullOrUndefined (SSECustomerKey), "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5), "RequestPayer" :: NullOrUndefined (RequestPayer), "PartNumber" :: NullOrUndefined (PartNumber) } -> { "Bucket" :: BucketName, "IfMatch" :: NullOrUndefined (IfMatch), "IfModifiedSince" :: NullOrUndefined (IfModifiedSince), "IfNoneMatch" :: NullOrUndefined (IfNoneMatch), "IfUnmodifiedSince" :: NullOrUndefined (IfUnmodifiedSince), "Key" :: ObjectKey, "Range" :: NullOrUndefined (Range), "ResponseCacheControl" :: NullOrUndefined (ResponseCacheControl), "ResponseContentDisposition" :: NullOrUndefined (ResponseContentDisposition), "ResponseContentEncoding" :: NullOrUndefined (ResponseContentEncoding), "ResponseContentLanguage" :: NullOrUndefined (ResponseContentLanguage), "ResponseContentType" :: NullOrUndefined (ResponseContentType), "ResponseExpires" :: NullOrUndefined (ResponseExpires), "VersionId" :: NullOrUndefined (ObjectVersionId), "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm), "SSECustomerKey" :: NullOrUndefined (SSECustomerKey), "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5), "RequestPayer" :: NullOrUndefined (RequestPayer), "PartNumber" :: NullOrUndefined (PartNumber) }) -> GetObjectRequest
```

Constructs GetObjectRequest's fields from required parameters

#### `GetObjectTaggingOutput`

``` purescript
newtype GetObjectTaggingOutput
  = GetObjectTaggingOutput { "VersionId" :: NullOrUndefined (ObjectVersionId), "TagSet" :: TagSet }
```

##### Instances
``` purescript
Newtype GetObjectTaggingOutput _
Generic GetObjectTaggingOutput _
Show GetObjectTaggingOutput
Decode GetObjectTaggingOutput
Encode GetObjectTaggingOutput
```

#### `newGetObjectTaggingOutput`

``` purescript
newGetObjectTaggingOutput :: TagSet -> GetObjectTaggingOutput
```

Constructs GetObjectTaggingOutput from required parameters

#### `newGetObjectTaggingOutput'`

``` purescript
newGetObjectTaggingOutput' :: TagSet -> ({ "VersionId" :: NullOrUndefined (ObjectVersionId), "TagSet" :: TagSet } -> { "VersionId" :: NullOrUndefined (ObjectVersionId), "TagSet" :: TagSet }) -> GetObjectTaggingOutput
```

Constructs GetObjectTaggingOutput's fields from required parameters

#### `GetObjectTaggingRequest`

``` purescript
newtype GetObjectTaggingRequest
  = GetObjectTaggingRequest { "Bucket" :: BucketName, "Key" :: ObjectKey, "VersionId" :: NullOrUndefined (ObjectVersionId) }
```

##### Instances
``` purescript
Newtype GetObjectTaggingRequest _
Generic GetObjectTaggingRequest _
Show GetObjectTaggingRequest
Decode GetObjectTaggingRequest
Encode GetObjectTaggingRequest
```

#### `newGetObjectTaggingRequest`

``` purescript
newGetObjectTaggingRequest :: BucketName -> ObjectKey -> GetObjectTaggingRequest
```

Constructs GetObjectTaggingRequest from required parameters

#### `newGetObjectTaggingRequest'`

``` purescript
newGetObjectTaggingRequest' :: BucketName -> ObjectKey -> ({ "Bucket" :: BucketName, "Key" :: ObjectKey, "VersionId" :: NullOrUndefined (ObjectVersionId) } -> { "Bucket" :: BucketName, "Key" :: ObjectKey, "VersionId" :: NullOrUndefined (ObjectVersionId) }) -> GetObjectTaggingRequest
```

Constructs GetObjectTaggingRequest's fields from required parameters

#### `GetObjectTorrentOutput`

``` purescript
newtype GetObjectTorrentOutput
  = GetObjectTorrentOutput { "Body" :: NullOrUndefined (Body), "RequestCharged" :: NullOrUndefined (RequestCharged) }
```

##### Instances
``` purescript
Newtype GetObjectTorrentOutput _
Generic GetObjectTorrentOutput _
Show GetObjectTorrentOutput
Decode GetObjectTorrentOutput
Encode GetObjectTorrentOutput
```

#### `newGetObjectTorrentOutput`

``` purescript
newGetObjectTorrentOutput :: GetObjectTorrentOutput
```

Constructs GetObjectTorrentOutput from required parameters

#### `newGetObjectTorrentOutput'`

``` purescript
newGetObjectTorrentOutput' :: ({ "Body" :: NullOrUndefined (Body), "RequestCharged" :: NullOrUndefined (RequestCharged) } -> { "Body" :: NullOrUndefined (Body), "RequestCharged" :: NullOrUndefined (RequestCharged) }) -> GetObjectTorrentOutput
```

Constructs GetObjectTorrentOutput's fields from required parameters

#### `GetObjectTorrentRequest`

``` purescript
newtype GetObjectTorrentRequest
  = GetObjectTorrentRequest { "Bucket" :: BucketName, "Key" :: ObjectKey, "RequestPayer" :: NullOrUndefined (RequestPayer) }
```

##### Instances
``` purescript
Newtype GetObjectTorrentRequest _
Generic GetObjectTorrentRequest _
Show GetObjectTorrentRequest
Decode GetObjectTorrentRequest
Encode GetObjectTorrentRequest
```

#### `newGetObjectTorrentRequest`

``` purescript
newGetObjectTorrentRequest :: BucketName -> ObjectKey -> GetObjectTorrentRequest
```

Constructs GetObjectTorrentRequest from required parameters

#### `newGetObjectTorrentRequest'`

``` purescript
newGetObjectTorrentRequest' :: BucketName -> ObjectKey -> ({ "Bucket" :: BucketName, "Key" :: ObjectKey, "RequestPayer" :: NullOrUndefined (RequestPayer) } -> { "Bucket" :: BucketName, "Key" :: ObjectKey, "RequestPayer" :: NullOrUndefined (RequestPayer) }) -> GetObjectTorrentRequest
```

Constructs GetObjectTorrentRequest's fields from required parameters

#### `GlacierJobParameters`

``` purescript
newtype GlacierJobParameters
  = GlacierJobParameters { "Tier" :: Tier }
```

##### Instances
``` purescript
Newtype GlacierJobParameters _
Generic GlacierJobParameters _
Show GlacierJobParameters
Decode GlacierJobParameters
Encode GlacierJobParameters
```

#### `newGlacierJobParameters`

``` purescript
newGlacierJobParameters :: Tier -> GlacierJobParameters
```

Constructs GlacierJobParameters from required parameters

#### `newGlacierJobParameters'`

``` purescript
newGlacierJobParameters' :: Tier -> ({ "Tier" :: Tier } -> { "Tier" :: Tier }) -> GlacierJobParameters
```

Constructs GlacierJobParameters's fields from required parameters

#### `Grant`

``` purescript
newtype Grant
  = Grant { "Grantee" :: NullOrUndefined (Grantee), "Permission" :: NullOrUndefined (Permission) }
```

##### Instances
``` purescript
Newtype Grant _
Generic Grant _
Show Grant
Decode Grant
Encode Grant
```

#### `newGrant`

``` purescript
newGrant :: Grant
```

Constructs Grant from required parameters

#### `newGrant'`

``` purescript
newGrant' :: ({ "Grantee" :: NullOrUndefined (Grantee), "Permission" :: NullOrUndefined (Permission) } -> { "Grantee" :: NullOrUndefined (Grantee), "Permission" :: NullOrUndefined (Permission) }) -> Grant
```

Constructs Grant's fields from required parameters

#### `GrantFullControl`

``` purescript
newtype GrantFullControl
  = GrantFullControl String
```

##### Instances
``` purescript
Newtype GrantFullControl _
Generic GrantFullControl _
Show GrantFullControl
Decode GrantFullControl
Encode GrantFullControl
```

#### `GrantRead`

``` purescript
newtype GrantRead
  = GrantRead String
```

##### Instances
``` purescript
Newtype GrantRead _
Generic GrantRead _
Show GrantRead
Decode GrantRead
Encode GrantRead
```

#### `GrantReadACP`

``` purescript
newtype GrantReadACP
  = GrantReadACP String
```

##### Instances
``` purescript
Newtype GrantReadACP _
Generic GrantReadACP _
Show GrantReadACP
Decode GrantReadACP
Encode GrantReadACP
```

#### `GrantWrite`

``` purescript
newtype GrantWrite
  = GrantWrite String
```

##### Instances
``` purescript
Newtype GrantWrite _
Generic GrantWrite _
Show GrantWrite
Decode GrantWrite
Encode GrantWrite
```

#### `GrantWriteACP`

``` purescript
newtype GrantWriteACP
  = GrantWriteACP String
```

##### Instances
``` purescript
Newtype GrantWriteACP _
Generic GrantWriteACP _
Show GrantWriteACP
Decode GrantWriteACP
Encode GrantWriteACP
```

#### `Grantee`

``` purescript
newtype Grantee
  = Grantee { "DisplayName" :: NullOrUndefined (DisplayName), "EmailAddress" :: NullOrUndefined (EmailAddress), "ID" :: NullOrUndefined (ID), "Type" :: Type, "URI" :: NullOrUndefined (URI) }
```

##### Instances
``` purescript
Newtype Grantee _
Generic Grantee _
Show Grantee
Decode Grantee
Encode Grantee
```

#### `newGrantee`

``` purescript
newGrantee :: Type -> Grantee
```

Constructs Grantee from required parameters

#### `newGrantee'`

``` purescript
newGrantee' :: Type -> ({ "DisplayName" :: NullOrUndefined (DisplayName), "EmailAddress" :: NullOrUndefined (EmailAddress), "ID" :: NullOrUndefined (ID), "Type" :: Type, "URI" :: NullOrUndefined (URI) } -> { "DisplayName" :: NullOrUndefined (DisplayName), "EmailAddress" :: NullOrUndefined (EmailAddress), "ID" :: NullOrUndefined (ID), "Type" :: Type, "URI" :: NullOrUndefined (URI) }) -> Grantee
```

Constructs Grantee's fields from required parameters

#### `Grants`

``` purescript
newtype Grants
  = Grants (Array Grant)
```

##### Instances
``` purescript
Newtype Grants _
Generic Grants _
Show Grants
Decode Grants
Encode Grants
```

#### `HeadBucketRequest`

``` purescript
newtype HeadBucketRequest
  = HeadBucketRequest { "Bucket" :: BucketName }
```

##### Instances
``` purescript
Newtype HeadBucketRequest _
Generic HeadBucketRequest _
Show HeadBucketRequest
Decode HeadBucketRequest
Encode HeadBucketRequest
```

#### `newHeadBucketRequest`

``` purescript
newHeadBucketRequest :: BucketName -> HeadBucketRequest
```

Constructs HeadBucketRequest from required parameters

#### `newHeadBucketRequest'`

``` purescript
newHeadBucketRequest' :: BucketName -> ({ "Bucket" :: BucketName } -> { "Bucket" :: BucketName }) -> HeadBucketRequest
```

Constructs HeadBucketRequest's fields from required parameters

#### `HeadObjectOutput`

``` purescript
newtype HeadObjectOutput
  = HeadObjectOutput { "DeleteMarker" :: NullOrUndefined (DeleteMarker), "AcceptRanges" :: NullOrUndefined (AcceptRanges), "Expiration" :: NullOrUndefined (Expiration), "Restore" :: NullOrUndefined (Restore), "LastModified" :: NullOrUndefined (LastModified), "ContentLength" :: NullOrUndefined (ContentLength), "ETag" :: NullOrUndefined (ETag), "MissingMeta" :: NullOrUndefined (MissingMeta), "VersionId" :: NullOrUndefined (ObjectVersionId), "CacheControl" :: NullOrUndefined (CacheControl), "ContentDisposition" :: NullOrUndefined (ContentDisposition), "ContentEncoding" :: NullOrUndefined (ContentEncoding), "ContentLanguage" :: NullOrUndefined (ContentLanguage), "ContentType" :: NullOrUndefined (ContentType), "Expires" :: NullOrUndefined (Expires), "WebsiteRedirectLocation" :: NullOrUndefined (WebsiteRedirectLocation), "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption), "Metadata" :: NullOrUndefined (Metadata), "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm), "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5), "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId), "StorageClass" :: NullOrUndefined (StorageClass), "RequestCharged" :: NullOrUndefined (RequestCharged), "ReplicationStatus" :: NullOrUndefined (ReplicationStatus), "PartsCount" :: NullOrUndefined (PartsCount) }
```

##### Instances
``` purescript
Newtype HeadObjectOutput _
Generic HeadObjectOutput _
Show HeadObjectOutput
Decode HeadObjectOutput
Encode HeadObjectOutput
```

#### `newHeadObjectOutput`

``` purescript
newHeadObjectOutput :: HeadObjectOutput
```

Constructs HeadObjectOutput from required parameters

#### `newHeadObjectOutput'`

``` purescript
newHeadObjectOutput' :: ({ "DeleteMarker" :: NullOrUndefined (DeleteMarker), "AcceptRanges" :: NullOrUndefined (AcceptRanges), "Expiration" :: NullOrUndefined (Expiration), "Restore" :: NullOrUndefined (Restore), "LastModified" :: NullOrUndefined (LastModified), "ContentLength" :: NullOrUndefined (ContentLength), "ETag" :: NullOrUndefined (ETag), "MissingMeta" :: NullOrUndefined (MissingMeta), "VersionId" :: NullOrUndefined (ObjectVersionId), "CacheControl" :: NullOrUndefined (CacheControl), "ContentDisposition" :: NullOrUndefined (ContentDisposition), "ContentEncoding" :: NullOrUndefined (ContentEncoding), "ContentLanguage" :: NullOrUndefined (ContentLanguage), "ContentType" :: NullOrUndefined (ContentType), "Expires" :: NullOrUndefined (Expires), "WebsiteRedirectLocation" :: NullOrUndefined (WebsiteRedirectLocation), "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption), "Metadata" :: NullOrUndefined (Metadata), "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm), "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5), "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId), "StorageClass" :: NullOrUndefined (StorageClass), "RequestCharged" :: NullOrUndefined (RequestCharged), "ReplicationStatus" :: NullOrUndefined (ReplicationStatus), "PartsCount" :: NullOrUndefined (PartsCount) } -> { "DeleteMarker" :: NullOrUndefined (DeleteMarker), "AcceptRanges" :: NullOrUndefined (AcceptRanges), "Expiration" :: NullOrUndefined (Expiration), "Restore" :: NullOrUndefined (Restore), "LastModified" :: NullOrUndefined (LastModified), "ContentLength" :: NullOrUndefined (ContentLength), "ETag" :: NullOrUndefined (ETag), "MissingMeta" :: NullOrUndefined (MissingMeta), "VersionId" :: NullOrUndefined (ObjectVersionId), "CacheControl" :: NullOrUndefined (CacheControl), "ContentDisposition" :: NullOrUndefined (ContentDisposition), "ContentEncoding" :: NullOrUndefined (ContentEncoding), "ContentLanguage" :: NullOrUndefined (ContentLanguage), "ContentType" :: NullOrUndefined (ContentType), "Expires" :: NullOrUndefined (Expires), "WebsiteRedirectLocation" :: NullOrUndefined (WebsiteRedirectLocation), "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption), "Metadata" :: NullOrUndefined (Metadata), "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm), "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5), "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId), "StorageClass" :: NullOrUndefined (StorageClass), "RequestCharged" :: NullOrUndefined (RequestCharged), "ReplicationStatus" :: NullOrUndefined (ReplicationStatus), "PartsCount" :: NullOrUndefined (PartsCount) }) -> HeadObjectOutput
```

Constructs HeadObjectOutput's fields from required parameters

#### `HeadObjectRequest`

``` purescript
newtype HeadObjectRequest
  = HeadObjectRequest { "Bucket" :: BucketName, "IfMatch" :: NullOrUndefined (IfMatch), "IfModifiedSince" :: NullOrUndefined (IfModifiedSince), "IfNoneMatch" :: NullOrUndefined (IfNoneMatch), "IfUnmodifiedSince" :: NullOrUndefined (IfUnmodifiedSince), "Key" :: ObjectKey, "Range" :: NullOrUndefined (Range), "VersionId" :: NullOrUndefined (ObjectVersionId), "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm), "SSECustomerKey" :: NullOrUndefined (SSECustomerKey), "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5), "RequestPayer" :: NullOrUndefined (RequestPayer), "PartNumber" :: NullOrUndefined (PartNumber) }
```

##### Instances
``` purescript
Newtype HeadObjectRequest _
Generic HeadObjectRequest _
Show HeadObjectRequest
Decode HeadObjectRequest
Encode HeadObjectRequest
```

#### `newHeadObjectRequest`

``` purescript
newHeadObjectRequest :: BucketName -> ObjectKey -> HeadObjectRequest
```

Constructs HeadObjectRequest from required parameters

#### `newHeadObjectRequest'`

``` purescript
newHeadObjectRequest' :: BucketName -> ObjectKey -> ({ "Bucket" :: BucketName, "IfMatch" :: NullOrUndefined (IfMatch), "IfModifiedSince" :: NullOrUndefined (IfModifiedSince), "IfNoneMatch" :: NullOrUndefined (IfNoneMatch), "IfUnmodifiedSince" :: NullOrUndefined (IfUnmodifiedSince), "Key" :: ObjectKey, "Range" :: NullOrUndefined (Range), "VersionId" :: NullOrUndefined (ObjectVersionId), "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm), "SSECustomerKey" :: NullOrUndefined (SSECustomerKey), "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5), "RequestPayer" :: NullOrUndefined (RequestPayer), "PartNumber" :: NullOrUndefined (PartNumber) } -> { "Bucket" :: BucketName, "IfMatch" :: NullOrUndefined (IfMatch), "IfModifiedSince" :: NullOrUndefined (IfModifiedSince), "IfNoneMatch" :: NullOrUndefined (IfNoneMatch), "IfUnmodifiedSince" :: NullOrUndefined (IfUnmodifiedSince), "Key" :: ObjectKey, "Range" :: NullOrUndefined (Range), "VersionId" :: NullOrUndefined (ObjectVersionId), "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm), "SSECustomerKey" :: NullOrUndefined (SSECustomerKey), "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5), "RequestPayer" :: NullOrUndefined (RequestPayer), "PartNumber" :: NullOrUndefined (PartNumber) }) -> HeadObjectRequest
```

Constructs HeadObjectRequest's fields from required parameters

#### `HostName`

``` purescript
newtype HostName
  = HostName String
```

##### Instances
``` purescript
Newtype HostName _
Generic HostName _
Show HostName
Decode HostName
Encode HostName
```

#### `HttpErrorCodeReturnedEquals`

``` purescript
newtype HttpErrorCodeReturnedEquals
  = HttpErrorCodeReturnedEquals String
```

##### Instances
``` purescript
Newtype HttpErrorCodeReturnedEquals _
Generic HttpErrorCodeReturnedEquals _
Show HttpErrorCodeReturnedEquals
Decode HttpErrorCodeReturnedEquals
Encode HttpErrorCodeReturnedEquals
```

#### `HttpRedirectCode`

``` purescript
newtype HttpRedirectCode
  = HttpRedirectCode String
```

##### Instances
``` purescript
Newtype HttpRedirectCode _
Generic HttpRedirectCode _
Show HttpRedirectCode
Decode HttpRedirectCode
Encode HttpRedirectCode
```

#### `ID`

``` purescript
newtype ID
  = ID String
```

##### Instances
``` purescript
Newtype ID _
Generic ID _
Show ID
Decode ID
Encode ID
```

#### `IfMatch`

``` purescript
newtype IfMatch
  = IfMatch String
```

##### Instances
``` purescript
Newtype IfMatch _
Generic IfMatch _
Show IfMatch
Decode IfMatch
Encode IfMatch
```

#### `IfModifiedSince`

``` purescript
newtype IfModifiedSince
  = IfModifiedSince Timestamp
```

##### Instances
``` purescript
Newtype IfModifiedSince _
Generic IfModifiedSince _
Show IfModifiedSince
Decode IfModifiedSince
Encode IfModifiedSince
```

#### `IfNoneMatch`

``` purescript
newtype IfNoneMatch
  = IfNoneMatch String
```

##### Instances
``` purescript
Newtype IfNoneMatch _
Generic IfNoneMatch _
Show IfNoneMatch
Decode IfNoneMatch
Encode IfNoneMatch
```

#### `IfUnmodifiedSince`

``` purescript
newtype IfUnmodifiedSince
  = IfUnmodifiedSince Timestamp
```

##### Instances
``` purescript
Newtype IfUnmodifiedSince _
Generic IfUnmodifiedSince _
Show IfUnmodifiedSince
Decode IfUnmodifiedSince
Encode IfUnmodifiedSince
```

#### `IndexDocument`

``` purescript
newtype IndexDocument
  = IndexDocument { "Suffix" :: Suffix }
```

##### Instances
``` purescript
Newtype IndexDocument _
Generic IndexDocument _
Show IndexDocument
Decode IndexDocument
Encode IndexDocument
```

#### `newIndexDocument`

``` purescript
newIndexDocument :: Suffix -> IndexDocument
```

Constructs IndexDocument from required parameters

#### `newIndexDocument'`

``` purescript
newIndexDocument' :: Suffix -> ({ "Suffix" :: Suffix } -> { "Suffix" :: Suffix }) -> IndexDocument
```

Constructs IndexDocument's fields from required parameters

#### `Initiated`

``` purescript
newtype Initiated
  = Initiated Timestamp
```

##### Instances
``` purescript
Newtype Initiated _
Generic Initiated _
Show Initiated
Decode Initiated
Encode Initiated
```

#### `Initiator`

``` purescript
newtype Initiator
  = Initiator { "ID" :: NullOrUndefined (ID), "DisplayName" :: NullOrUndefined (DisplayName) }
```

##### Instances
``` purescript
Newtype Initiator _
Generic Initiator _
Show Initiator
Decode Initiator
Encode Initiator
```

#### `newInitiator`

``` purescript
newInitiator :: Initiator
```

Constructs Initiator from required parameters

#### `newInitiator'`

``` purescript
newInitiator' :: ({ "ID" :: NullOrUndefined (ID), "DisplayName" :: NullOrUndefined (DisplayName) } -> { "ID" :: NullOrUndefined (ID), "DisplayName" :: NullOrUndefined (DisplayName) }) -> Initiator
```

Constructs Initiator's fields from required parameters

#### `InputSerialization`

``` purescript
newtype InputSerialization
  = InputSerialization { "CSV" :: NullOrUndefined (CSVInput) }
```

Describes the serialization format of the object.

##### Instances
``` purescript
Newtype InputSerialization _
Generic InputSerialization _
Show InputSerialization
Decode InputSerialization
Encode InputSerialization
```

#### `newInputSerialization`

``` purescript
newInputSerialization :: InputSerialization
```

Constructs InputSerialization from required parameters

#### `newInputSerialization'`

``` purescript
newInputSerialization' :: ({ "CSV" :: NullOrUndefined (CSVInput) } -> { "CSV" :: NullOrUndefined (CSVInput) }) -> InputSerialization
```

Constructs InputSerialization's fields from required parameters

#### `InventoryConfiguration`

``` purescript
newtype InventoryConfiguration
  = InventoryConfiguration { "Destination" :: InventoryDestination, "IsEnabled" :: IsEnabled, "Filter" :: NullOrUndefined (InventoryFilter), "Id" :: InventoryId, "IncludedObjectVersions" :: InventoryIncludedObjectVersions, "OptionalFields" :: NullOrUndefined (InventoryOptionalFields), "Schedule" :: InventorySchedule }
```

##### Instances
``` purescript
Newtype InventoryConfiguration _
Generic InventoryConfiguration _
Show InventoryConfiguration
Decode InventoryConfiguration
Encode InventoryConfiguration
```

#### `newInventoryConfiguration`

``` purescript
newInventoryConfiguration :: InventoryDestination -> InventoryId -> InventoryIncludedObjectVersions -> IsEnabled -> InventorySchedule -> InventoryConfiguration
```

Constructs InventoryConfiguration from required parameters

#### `newInventoryConfiguration'`

``` purescript
newInventoryConfiguration' :: InventoryDestination -> InventoryId -> InventoryIncludedObjectVersions -> IsEnabled -> InventorySchedule -> ({ "Destination" :: InventoryDestination, "IsEnabled" :: IsEnabled, "Filter" :: NullOrUndefined (InventoryFilter), "Id" :: InventoryId, "IncludedObjectVersions" :: InventoryIncludedObjectVersions, "OptionalFields" :: NullOrUndefined (InventoryOptionalFields), "Schedule" :: InventorySchedule } -> { "Destination" :: InventoryDestination, "IsEnabled" :: IsEnabled, "Filter" :: NullOrUndefined (InventoryFilter), "Id" :: InventoryId, "IncludedObjectVersions" :: InventoryIncludedObjectVersions, "OptionalFields" :: NullOrUndefined (InventoryOptionalFields), "Schedule" :: InventorySchedule }) -> InventoryConfiguration
```

Constructs InventoryConfiguration's fields from required parameters

#### `InventoryConfigurationList`

``` purescript
newtype InventoryConfigurationList
  = InventoryConfigurationList (Array InventoryConfiguration)
```

##### Instances
``` purescript
Newtype InventoryConfigurationList _
Generic InventoryConfigurationList _
Show InventoryConfigurationList
Decode InventoryConfigurationList
Encode InventoryConfigurationList
```

#### `InventoryDestination`

``` purescript
newtype InventoryDestination
  = InventoryDestination { "S3BucketDestination" :: InventoryS3BucketDestination }
```

##### Instances
``` purescript
Newtype InventoryDestination _
Generic InventoryDestination _
Show InventoryDestination
Decode InventoryDestination
Encode InventoryDestination
```

#### `newInventoryDestination`

``` purescript
newInventoryDestination :: InventoryS3BucketDestination -> InventoryDestination
```

Constructs InventoryDestination from required parameters

#### `newInventoryDestination'`

``` purescript
newInventoryDestination' :: InventoryS3BucketDestination -> ({ "S3BucketDestination" :: InventoryS3BucketDestination } -> { "S3BucketDestination" :: InventoryS3BucketDestination }) -> InventoryDestination
```

Constructs InventoryDestination's fields from required parameters

#### `InventoryEncryption`

``` purescript
newtype InventoryEncryption
  = InventoryEncryption { "SSES3" :: NullOrUndefined (SSES3), "SSEKMS" :: NullOrUndefined (SSEKMS) }
```

Contains the type of server-side encryption used to encrypt the inventory results.

##### Instances
``` purescript
Newtype InventoryEncryption _
Generic InventoryEncryption _
Show InventoryEncryption
Decode InventoryEncryption
Encode InventoryEncryption
```

#### `newInventoryEncryption`

``` purescript
newInventoryEncryption :: InventoryEncryption
```

Constructs InventoryEncryption from required parameters

#### `newInventoryEncryption'`

``` purescript
newInventoryEncryption' :: ({ "SSES3" :: NullOrUndefined (SSES3), "SSEKMS" :: NullOrUndefined (SSEKMS) } -> { "SSES3" :: NullOrUndefined (SSES3), "SSEKMS" :: NullOrUndefined (SSEKMS) }) -> InventoryEncryption
```

Constructs InventoryEncryption's fields from required parameters

#### `InventoryFilter`

``` purescript
newtype InventoryFilter
  = InventoryFilter { "Prefix" :: Prefix }
```

##### Instances
``` purescript
Newtype InventoryFilter _
Generic InventoryFilter _
Show InventoryFilter
Decode InventoryFilter
Encode InventoryFilter
```

#### `newInventoryFilter`

``` purescript
newInventoryFilter :: Prefix -> InventoryFilter
```

Constructs InventoryFilter from required parameters

#### `newInventoryFilter'`

``` purescript
newInventoryFilter' :: Prefix -> ({ "Prefix" :: Prefix } -> { "Prefix" :: Prefix }) -> InventoryFilter
```

Constructs InventoryFilter's fields from required parameters

#### `InventoryFormat`

``` purescript
newtype InventoryFormat
  = InventoryFormat String
```

##### Instances
``` purescript
Newtype InventoryFormat _
Generic InventoryFormat _
Show InventoryFormat
Decode InventoryFormat
Encode InventoryFormat
```

#### `InventoryFrequency`

``` purescript
newtype InventoryFrequency
  = InventoryFrequency String
```

##### Instances
``` purescript
Newtype InventoryFrequency _
Generic InventoryFrequency _
Show InventoryFrequency
Decode InventoryFrequency
Encode InventoryFrequency
```

#### `InventoryId`

``` purescript
newtype InventoryId
  = InventoryId String
```

##### Instances
``` purescript
Newtype InventoryId _
Generic InventoryId _
Show InventoryId
Decode InventoryId
Encode InventoryId
```

#### `InventoryIncludedObjectVersions`

``` purescript
newtype InventoryIncludedObjectVersions
  = InventoryIncludedObjectVersions String
```

##### Instances
``` purescript
Newtype InventoryIncludedObjectVersions _
Generic InventoryIncludedObjectVersions _
Show InventoryIncludedObjectVersions
Decode InventoryIncludedObjectVersions
Encode InventoryIncludedObjectVersions
```

#### `InventoryOptionalField`

``` purescript
newtype InventoryOptionalField
  = InventoryOptionalField String
```

##### Instances
``` purescript
Newtype InventoryOptionalField _
Generic InventoryOptionalField _
Show InventoryOptionalField
Decode InventoryOptionalField
Encode InventoryOptionalField
```

#### `InventoryOptionalFields`

``` purescript
newtype InventoryOptionalFields
  = InventoryOptionalFields (Array InventoryOptionalField)
```

##### Instances
``` purescript
Newtype InventoryOptionalFields _
Generic InventoryOptionalFields _
Show InventoryOptionalFields
Decode InventoryOptionalFields
Encode InventoryOptionalFields
```

#### `InventoryS3BucketDestination`

``` purescript
newtype InventoryS3BucketDestination
  = InventoryS3BucketDestination { "AccountId" :: NullOrUndefined (AccountId), "Bucket" :: BucketName, "Format" :: InventoryFormat, "Prefix" :: NullOrUndefined (Prefix), "Encryption" :: NullOrUndefined (InventoryEncryption) }
```

##### Instances
``` purescript
Newtype InventoryS3BucketDestination _
Generic InventoryS3BucketDestination _
Show InventoryS3BucketDestination
Decode InventoryS3BucketDestination
Encode InventoryS3BucketDestination
```

#### `newInventoryS3BucketDestination`

``` purescript
newInventoryS3BucketDestination :: BucketName -> InventoryFormat -> InventoryS3BucketDestination
```

Constructs InventoryS3BucketDestination from required parameters

#### `newInventoryS3BucketDestination'`

``` purescript
newInventoryS3BucketDestination' :: BucketName -> InventoryFormat -> ({ "AccountId" :: NullOrUndefined (AccountId), "Bucket" :: BucketName, "Format" :: InventoryFormat, "Prefix" :: NullOrUndefined (Prefix), "Encryption" :: NullOrUndefined (InventoryEncryption) } -> { "AccountId" :: NullOrUndefined (AccountId), "Bucket" :: BucketName, "Format" :: InventoryFormat, "Prefix" :: NullOrUndefined (Prefix), "Encryption" :: NullOrUndefined (InventoryEncryption) }) -> InventoryS3BucketDestination
```

Constructs InventoryS3BucketDestination's fields from required parameters

#### `InventorySchedule`

``` purescript
newtype InventorySchedule
  = InventorySchedule { "Frequency" :: InventoryFrequency }
```

##### Instances
``` purescript
Newtype InventorySchedule _
Generic InventorySchedule _
Show InventorySchedule
Decode InventorySchedule
Encode InventorySchedule
```

#### `newInventorySchedule`

``` purescript
newInventorySchedule :: InventoryFrequency -> InventorySchedule
```

Constructs InventorySchedule from required parameters

#### `newInventorySchedule'`

``` purescript
newInventorySchedule' :: InventoryFrequency -> ({ "Frequency" :: InventoryFrequency } -> { "Frequency" :: InventoryFrequency }) -> InventorySchedule
```

Constructs InventorySchedule's fields from required parameters

#### `IsEnabled`

``` purescript
newtype IsEnabled
  = IsEnabled Boolean
```

##### Instances
``` purescript
Newtype IsEnabled _
Generic IsEnabled _
Show IsEnabled
Decode IsEnabled
Encode IsEnabled
```

#### `IsLatest`

``` purescript
newtype IsLatest
  = IsLatest Boolean
```

##### Instances
``` purescript
Newtype IsLatest _
Generic IsLatest _
Show IsLatest
Decode IsLatest
Encode IsLatest
```

#### `IsTruncated`

``` purescript
newtype IsTruncated
  = IsTruncated Boolean
```

##### Instances
``` purescript
Newtype IsTruncated _
Generic IsTruncated _
Show IsTruncated
Decode IsTruncated
Encode IsTruncated
```

#### `KMSContext`

``` purescript
newtype KMSContext
  = KMSContext String
```

##### Instances
``` purescript
Newtype KMSContext _
Generic KMSContext _
Show KMSContext
Decode KMSContext
Encode KMSContext
```

#### `KeyCount`

``` purescript
newtype KeyCount
  = KeyCount Int
```

##### Instances
``` purescript
Newtype KeyCount _
Generic KeyCount _
Show KeyCount
Decode KeyCount
Encode KeyCount
```

#### `KeyMarker`

``` purescript
newtype KeyMarker
  = KeyMarker String
```

##### Instances
``` purescript
Newtype KeyMarker _
Generic KeyMarker _
Show KeyMarker
Decode KeyMarker
Encode KeyMarker
```

#### `KeyPrefixEquals`

``` purescript
newtype KeyPrefixEquals
  = KeyPrefixEquals String
```

##### Instances
``` purescript
Newtype KeyPrefixEquals _
Generic KeyPrefixEquals _
Show KeyPrefixEquals
Decode KeyPrefixEquals
Encode KeyPrefixEquals
```

#### `LambdaFunctionArn`

``` purescript
newtype LambdaFunctionArn
  = LambdaFunctionArn String
```

##### Instances
``` purescript
Newtype LambdaFunctionArn _
Generic LambdaFunctionArn _
Show LambdaFunctionArn
Decode LambdaFunctionArn
Encode LambdaFunctionArn
```

#### `LambdaFunctionConfiguration`

``` purescript
newtype LambdaFunctionConfiguration
  = LambdaFunctionConfiguration { "Id" :: NullOrUndefined (NotificationId), "LambdaFunctionArn" :: LambdaFunctionArn, "Events" :: EventList, "Filter" :: NullOrUndefined (NotificationConfigurationFilter) }
```

Container for specifying the AWS Lambda notification configuration.

##### Instances
``` purescript
Newtype LambdaFunctionConfiguration _
Generic LambdaFunctionConfiguration _
Show LambdaFunctionConfiguration
Decode LambdaFunctionConfiguration
Encode LambdaFunctionConfiguration
```

#### `newLambdaFunctionConfiguration`

``` purescript
newLambdaFunctionConfiguration :: EventList -> LambdaFunctionArn -> LambdaFunctionConfiguration
```

Constructs LambdaFunctionConfiguration from required parameters

#### `newLambdaFunctionConfiguration'`

``` purescript
newLambdaFunctionConfiguration' :: EventList -> LambdaFunctionArn -> ({ "Id" :: NullOrUndefined (NotificationId), "LambdaFunctionArn" :: LambdaFunctionArn, "Events" :: EventList, "Filter" :: NullOrUndefined (NotificationConfigurationFilter) } -> { "Id" :: NullOrUndefined (NotificationId), "LambdaFunctionArn" :: LambdaFunctionArn, "Events" :: EventList, "Filter" :: NullOrUndefined (NotificationConfigurationFilter) }) -> LambdaFunctionConfiguration
```

Constructs LambdaFunctionConfiguration's fields from required parameters

#### `LambdaFunctionConfigurationList`

``` purescript
newtype LambdaFunctionConfigurationList
  = LambdaFunctionConfigurationList (Array LambdaFunctionConfiguration)
```

##### Instances
``` purescript
Newtype LambdaFunctionConfigurationList _
Generic LambdaFunctionConfigurationList _
Show LambdaFunctionConfigurationList
Decode LambdaFunctionConfigurationList
Encode LambdaFunctionConfigurationList
```

#### `LastModified`

``` purescript
newtype LastModified
  = LastModified Timestamp
```

##### Instances
``` purescript
Newtype LastModified _
Generic LastModified _
Show LastModified
Decode LastModified
Encode LastModified
```

#### `LifecycleConfiguration`

``` purescript
newtype LifecycleConfiguration
  = LifecycleConfiguration { "Rules" :: Rules }
```

##### Instances
``` purescript
Newtype LifecycleConfiguration _
Generic LifecycleConfiguration _
Show LifecycleConfiguration
Decode LifecycleConfiguration
Encode LifecycleConfiguration
```

#### `newLifecycleConfiguration`

``` purescript
newLifecycleConfiguration :: Rules -> LifecycleConfiguration
```

Constructs LifecycleConfiguration from required parameters

#### `newLifecycleConfiguration'`

``` purescript
newLifecycleConfiguration' :: Rules -> ({ "Rules" :: Rules } -> { "Rules" :: Rules }) -> LifecycleConfiguration
```

Constructs LifecycleConfiguration's fields from required parameters

#### `LifecycleExpiration`

``` purescript
newtype LifecycleExpiration
  = LifecycleExpiration { "Date" :: NullOrUndefined (Date), "Days" :: NullOrUndefined (Days), "ExpiredObjectDeleteMarker" :: NullOrUndefined (ExpiredObjectDeleteMarker) }
```

##### Instances
``` purescript
Newtype LifecycleExpiration _
Generic LifecycleExpiration _
Show LifecycleExpiration
Decode LifecycleExpiration
Encode LifecycleExpiration
```

#### `newLifecycleExpiration`

``` purescript
newLifecycleExpiration :: LifecycleExpiration
```

Constructs LifecycleExpiration from required parameters

#### `newLifecycleExpiration'`

``` purescript
newLifecycleExpiration' :: ({ "Date" :: NullOrUndefined (Date), "Days" :: NullOrUndefined (Days), "ExpiredObjectDeleteMarker" :: NullOrUndefined (ExpiredObjectDeleteMarker) } -> { "Date" :: NullOrUndefined (Date), "Days" :: NullOrUndefined (Days), "ExpiredObjectDeleteMarker" :: NullOrUndefined (ExpiredObjectDeleteMarker) }) -> LifecycleExpiration
```

Constructs LifecycleExpiration's fields from required parameters

#### `LifecycleRule`

``` purescript
newtype LifecycleRule
  = LifecycleRule { "Expiration" :: NullOrUndefined (LifecycleExpiration), "ID" :: NullOrUndefined (ID), "Prefix" :: NullOrUndefined (Prefix), "Filter" :: NullOrUndefined (LifecycleRuleFilter), "Status" :: ExpirationStatus, "Transitions" :: NullOrUndefined (TransitionList), "NoncurrentVersionTransitions" :: NullOrUndefined (NoncurrentVersionTransitionList), "NoncurrentVersionExpiration" :: NullOrUndefined (NoncurrentVersionExpiration), "AbortIncompleteMultipartUpload" :: NullOrUndefined (AbortIncompleteMultipartUpload) }
```

##### Instances
``` purescript
Newtype LifecycleRule _
Generic LifecycleRule _
Show LifecycleRule
Decode LifecycleRule
Encode LifecycleRule
```

#### `newLifecycleRule`

``` purescript
newLifecycleRule :: ExpirationStatus -> LifecycleRule
```

Constructs LifecycleRule from required parameters

#### `newLifecycleRule'`

``` purescript
newLifecycleRule' :: ExpirationStatus -> ({ "Expiration" :: NullOrUndefined (LifecycleExpiration), "ID" :: NullOrUndefined (ID), "Prefix" :: NullOrUndefined (Prefix), "Filter" :: NullOrUndefined (LifecycleRuleFilter), "Status" :: ExpirationStatus, "Transitions" :: NullOrUndefined (TransitionList), "NoncurrentVersionTransitions" :: NullOrUndefined (NoncurrentVersionTransitionList), "NoncurrentVersionExpiration" :: NullOrUndefined (NoncurrentVersionExpiration), "AbortIncompleteMultipartUpload" :: NullOrUndefined (AbortIncompleteMultipartUpload) } -> { "Expiration" :: NullOrUndefined (LifecycleExpiration), "ID" :: NullOrUndefined (ID), "Prefix" :: NullOrUndefined (Prefix), "Filter" :: NullOrUndefined (LifecycleRuleFilter), "Status" :: ExpirationStatus, "Transitions" :: NullOrUndefined (TransitionList), "NoncurrentVersionTransitions" :: NullOrUndefined (NoncurrentVersionTransitionList), "NoncurrentVersionExpiration" :: NullOrUndefined (NoncurrentVersionExpiration), "AbortIncompleteMultipartUpload" :: NullOrUndefined (AbortIncompleteMultipartUpload) }) -> LifecycleRule
```

Constructs LifecycleRule's fields from required parameters

#### `LifecycleRuleAndOperator`

``` purescript
newtype LifecycleRuleAndOperator
  = LifecycleRuleAndOperator { "Prefix" :: NullOrUndefined (Prefix), "Tags" :: NullOrUndefined (TagSet) }
```

This is used in a Lifecycle Rule Filter to apply a logical AND to two or more predicates. The Lifecycle Rule will apply to any object matching all of the predicates configured inside the And operator.

##### Instances
``` purescript
Newtype LifecycleRuleAndOperator _
Generic LifecycleRuleAndOperator _
Show LifecycleRuleAndOperator
Decode LifecycleRuleAndOperator
Encode LifecycleRuleAndOperator
```

#### `newLifecycleRuleAndOperator`

``` purescript
newLifecycleRuleAndOperator :: LifecycleRuleAndOperator
```

Constructs LifecycleRuleAndOperator from required parameters

#### `newLifecycleRuleAndOperator'`

``` purescript
newLifecycleRuleAndOperator' :: ({ "Prefix" :: NullOrUndefined (Prefix), "Tags" :: NullOrUndefined (TagSet) } -> { "Prefix" :: NullOrUndefined (Prefix), "Tags" :: NullOrUndefined (TagSet) }) -> LifecycleRuleAndOperator
```

Constructs LifecycleRuleAndOperator's fields from required parameters

#### `LifecycleRuleFilter`

``` purescript
newtype LifecycleRuleFilter
  = LifecycleRuleFilter { "Prefix" :: NullOrUndefined (Prefix), "Tag" :: NullOrUndefined (Tag), "And" :: NullOrUndefined (LifecycleRuleAndOperator) }
```

The Filter is used to identify objects that a Lifecycle Rule applies to. A Filter must have exactly one of Prefix, Tag, or And specified.

##### Instances
``` purescript
Newtype LifecycleRuleFilter _
Generic LifecycleRuleFilter _
Show LifecycleRuleFilter
Decode LifecycleRuleFilter
Encode LifecycleRuleFilter
```

#### `newLifecycleRuleFilter`

``` purescript
newLifecycleRuleFilter :: LifecycleRuleFilter
```

Constructs LifecycleRuleFilter from required parameters

#### `newLifecycleRuleFilter'`

``` purescript
newLifecycleRuleFilter' :: ({ "Prefix" :: NullOrUndefined (Prefix), "Tag" :: NullOrUndefined (Tag), "And" :: NullOrUndefined (LifecycleRuleAndOperator) } -> { "Prefix" :: NullOrUndefined (Prefix), "Tag" :: NullOrUndefined (Tag), "And" :: NullOrUndefined (LifecycleRuleAndOperator) }) -> LifecycleRuleFilter
```

Constructs LifecycleRuleFilter's fields from required parameters

#### `LifecycleRules`

``` purescript
newtype LifecycleRules
  = LifecycleRules (Array LifecycleRule)
```

##### Instances
``` purescript
Newtype LifecycleRules _
Generic LifecycleRules _
Show LifecycleRules
Decode LifecycleRules
Encode LifecycleRules
```

#### `ListBucketAnalyticsConfigurationsOutput`

``` purescript
newtype ListBucketAnalyticsConfigurationsOutput
  = ListBucketAnalyticsConfigurationsOutput { "IsTruncated" :: NullOrUndefined (IsTruncated), "ContinuationToken" :: NullOrUndefined (Token), "NextContinuationToken" :: NullOrUndefined (NextToken), "AnalyticsConfigurationList" :: NullOrUndefined (AnalyticsConfigurationList) }
```

##### Instances
``` purescript
Newtype ListBucketAnalyticsConfigurationsOutput _
Generic ListBucketAnalyticsConfigurationsOutput _
Show ListBucketAnalyticsConfigurationsOutput
Decode ListBucketAnalyticsConfigurationsOutput
Encode ListBucketAnalyticsConfigurationsOutput
```

#### `newListBucketAnalyticsConfigurationsOutput`

``` purescript
newListBucketAnalyticsConfigurationsOutput :: ListBucketAnalyticsConfigurationsOutput
```

Constructs ListBucketAnalyticsConfigurationsOutput from required parameters

#### `newListBucketAnalyticsConfigurationsOutput'`

``` purescript
newListBucketAnalyticsConfigurationsOutput' :: ({ "IsTruncated" :: NullOrUndefined (IsTruncated), "ContinuationToken" :: NullOrUndefined (Token), "NextContinuationToken" :: NullOrUndefined (NextToken), "AnalyticsConfigurationList" :: NullOrUndefined (AnalyticsConfigurationList) } -> { "IsTruncated" :: NullOrUndefined (IsTruncated), "ContinuationToken" :: NullOrUndefined (Token), "NextContinuationToken" :: NullOrUndefined (NextToken), "AnalyticsConfigurationList" :: NullOrUndefined (AnalyticsConfigurationList) }) -> ListBucketAnalyticsConfigurationsOutput
```

Constructs ListBucketAnalyticsConfigurationsOutput's fields from required parameters

#### `ListBucketAnalyticsConfigurationsRequest`

``` purescript
newtype ListBucketAnalyticsConfigurationsRequest
  = ListBucketAnalyticsConfigurationsRequest { "Bucket" :: BucketName, "ContinuationToken" :: NullOrUndefined (Token) }
```

##### Instances
``` purescript
Newtype ListBucketAnalyticsConfigurationsRequest _
Generic ListBucketAnalyticsConfigurationsRequest _
Show ListBucketAnalyticsConfigurationsRequest
Decode ListBucketAnalyticsConfigurationsRequest
Encode ListBucketAnalyticsConfigurationsRequest
```

#### `newListBucketAnalyticsConfigurationsRequest`

``` purescript
newListBucketAnalyticsConfigurationsRequest :: BucketName -> ListBucketAnalyticsConfigurationsRequest
```

Constructs ListBucketAnalyticsConfigurationsRequest from required parameters

#### `newListBucketAnalyticsConfigurationsRequest'`

``` purescript
newListBucketAnalyticsConfigurationsRequest' :: BucketName -> ({ "Bucket" :: BucketName, "ContinuationToken" :: NullOrUndefined (Token) } -> { "Bucket" :: BucketName, "ContinuationToken" :: NullOrUndefined (Token) }) -> ListBucketAnalyticsConfigurationsRequest
```

Constructs ListBucketAnalyticsConfigurationsRequest's fields from required parameters

#### `ListBucketInventoryConfigurationsOutput`

``` purescript
newtype ListBucketInventoryConfigurationsOutput
  = ListBucketInventoryConfigurationsOutput { "ContinuationToken" :: NullOrUndefined (Token), "InventoryConfigurationList" :: NullOrUndefined (InventoryConfigurationList), "IsTruncated" :: NullOrUndefined (IsTruncated), "NextContinuationToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListBucketInventoryConfigurationsOutput _
Generic ListBucketInventoryConfigurationsOutput _
Show ListBucketInventoryConfigurationsOutput
Decode ListBucketInventoryConfigurationsOutput
Encode ListBucketInventoryConfigurationsOutput
```

#### `newListBucketInventoryConfigurationsOutput`

``` purescript
newListBucketInventoryConfigurationsOutput :: ListBucketInventoryConfigurationsOutput
```

Constructs ListBucketInventoryConfigurationsOutput from required parameters

#### `newListBucketInventoryConfigurationsOutput'`

``` purescript
newListBucketInventoryConfigurationsOutput' :: ({ "ContinuationToken" :: NullOrUndefined (Token), "InventoryConfigurationList" :: NullOrUndefined (InventoryConfigurationList), "IsTruncated" :: NullOrUndefined (IsTruncated), "NextContinuationToken" :: NullOrUndefined (NextToken) } -> { "ContinuationToken" :: NullOrUndefined (Token), "InventoryConfigurationList" :: NullOrUndefined (InventoryConfigurationList), "IsTruncated" :: NullOrUndefined (IsTruncated), "NextContinuationToken" :: NullOrUndefined (NextToken) }) -> ListBucketInventoryConfigurationsOutput
```

Constructs ListBucketInventoryConfigurationsOutput's fields from required parameters

#### `ListBucketInventoryConfigurationsRequest`

``` purescript
newtype ListBucketInventoryConfigurationsRequest
  = ListBucketInventoryConfigurationsRequest { "Bucket" :: BucketName, "ContinuationToken" :: NullOrUndefined (Token) }
```

##### Instances
``` purescript
Newtype ListBucketInventoryConfigurationsRequest _
Generic ListBucketInventoryConfigurationsRequest _
Show ListBucketInventoryConfigurationsRequest
Decode ListBucketInventoryConfigurationsRequest
Encode ListBucketInventoryConfigurationsRequest
```

#### `newListBucketInventoryConfigurationsRequest`

``` purescript
newListBucketInventoryConfigurationsRequest :: BucketName -> ListBucketInventoryConfigurationsRequest
```

Constructs ListBucketInventoryConfigurationsRequest from required parameters

#### `newListBucketInventoryConfigurationsRequest'`

``` purescript
newListBucketInventoryConfigurationsRequest' :: BucketName -> ({ "Bucket" :: BucketName, "ContinuationToken" :: NullOrUndefined (Token) } -> { "Bucket" :: BucketName, "ContinuationToken" :: NullOrUndefined (Token) }) -> ListBucketInventoryConfigurationsRequest
```

Constructs ListBucketInventoryConfigurationsRequest's fields from required parameters

#### `ListBucketMetricsConfigurationsOutput`

``` purescript
newtype ListBucketMetricsConfigurationsOutput
  = ListBucketMetricsConfigurationsOutput { "IsTruncated" :: NullOrUndefined (IsTruncated), "ContinuationToken" :: NullOrUndefined (Token), "NextContinuationToken" :: NullOrUndefined (NextToken), "MetricsConfigurationList" :: NullOrUndefined (MetricsConfigurationList) }
```

##### Instances
``` purescript
Newtype ListBucketMetricsConfigurationsOutput _
Generic ListBucketMetricsConfigurationsOutput _
Show ListBucketMetricsConfigurationsOutput
Decode ListBucketMetricsConfigurationsOutput
Encode ListBucketMetricsConfigurationsOutput
```

#### `newListBucketMetricsConfigurationsOutput`

``` purescript
newListBucketMetricsConfigurationsOutput :: ListBucketMetricsConfigurationsOutput
```

Constructs ListBucketMetricsConfigurationsOutput from required parameters

#### `newListBucketMetricsConfigurationsOutput'`

``` purescript
newListBucketMetricsConfigurationsOutput' :: ({ "IsTruncated" :: NullOrUndefined (IsTruncated), "ContinuationToken" :: NullOrUndefined (Token), "NextContinuationToken" :: NullOrUndefined (NextToken), "MetricsConfigurationList" :: NullOrUndefined (MetricsConfigurationList) } -> { "IsTruncated" :: NullOrUndefined (IsTruncated), "ContinuationToken" :: NullOrUndefined (Token), "NextContinuationToken" :: NullOrUndefined (NextToken), "MetricsConfigurationList" :: NullOrUndefined (MetricsConfigurationList) }) -> ListBucketMetricsConfigurationsOutput
```

Constructs ListBucketMetricsConfigurationsOutput's fields from required parameters

#### `ListBucketMetricsConfigurationsRequest`

``` purescript
newtype ListBucketMetricsConfigurationsRequest
  = ListBucketMetricsConfigurationsRequest { "Bucket" :: BucketName, "ContinuationToken" :: NullOrUndefined (Token) }
```

##### Instances
``` purescript
Newtype ListBucketMetricsConfigurationsRequest _
Generic ListBucketMetricsConfigurationsRequest _
Show ListBucketMetricsConfigurationsRequest
Decode ListBucketMetricsConfigurationsRequest
Encode ListBucketMetricsConfigurationsRequest
```

#### `newListBucketMetricsConfigurationsRequest`

``` purescript
newListBucketMetricsConfigurationsRequest :: BucketName -> ListBucketMetricsConfigurationsRequest
```

Constructs ListBucketMetricsConfigurationsRequest from required parameters

#### `newListBucketMetricsConfigurationsRequest'`

``` purescript
newListBucketMetricsConfigurationsRequest' :: BucketName -> ({ "Bucket" :: BucketName, "ContinuationToken" :: NullOrUndefined (Token) } -> { "Bucket" :: BucketName, "ContinuationToken" :: NullOrUndefined (Token) }) -> ListBucketMetricsConfigurationsRequest
```

Constructs ListBucketMetricsConfigurationsRequest's fields from required parameters

#### `ListBucketsOutput`

``` purescript
newtype ListBucketsOutput
  = ListBucketsOutput { "Buckets" :: NullOrUndefined (Buckets), "Owner" :: NullOrUndefined (Owner) }
```

##### Instances
``` purescript
Newtype ListBucketsOutput _
Generic ListBucketsOutput _
Show ListBucketsOutput
Decode ListBucketsOutput
Encode ListBucketsOutput
```

#### `newListBucketsOutput`

``` purescript
newListBucketsOutput :: ListBucketsOutput
```

Constructs ListBucketsOutput from required parameters

#### `newListBucketsOutput'`

``` purescript
newListBucketsOutput' :: ({ "Buckets" :: NullOrUndefined (Buckets), "Owner" :: NullOrUndefined (Owner) } -> { "Buckets" :: NullOrUndefined (Buckets), "Owner" :: NullOrUndefined (Owner) }) -> ListBucketsOutput
```

Constructs ListBucketsOutput's fields from required parameters

#### `ListMultipartUploadsOutput`

``` purescript
newtype ListMultipartUploadsOutput
  = ListMultipartUploadsOutput { "Bucket" :: NullOrUndefined (BucketName), "KeyMarker" :: NullOrUndefined (KeyMarker), "UploadIdMarker" :: NullOrUndefined (UploadIdMarker), "NextKeyMarker" :: NullOrUndefined (NextKeyMarker), "Prefix" :: NullOrUndefined (Prefix), "Delimiter" :: NullOrUndefined (Delimiter), "NextUploadIdMarker" :: NullOrUndefined (NextUploadIdMarker), "MaxUploads" :: NullOrUndefined (MaxUploads), "IsTruncated" :: NullOrUndefined (IsTruncated), "Uploads" :: NullOrUndefined (MultipartUploadList), "CommonPrefixes" :: NullOrUndefined (CommonPrefixList), "EncodingType" :: NullOrUndefined (EncodingType) }
```

##### Instances
``` purescript
Newtype ListMultipartUploadsOutput _
Generic ListMultipartUploadsOutput _
Show ListMultipartUploadsOutput
Decode ListMultipartUploadsOutput
Encode ListMultipartUploadsOutput
```

#### `newListMultipartUploadsOutput`

``` purescript
newListMultipartUploadsOutput :: ListMultipartUploadsOutput
```

Constructs ListMultipartUploadsOutput from required parameters

#### `newListMultipartUploadsOutput'`

``` purescript
newListMultipartUploadsOutput' :: ({ "Bucket" :: NullOrUndefined (BucketName), "KeyMarker" :: NullOrUndefined (KeyMarker), "UploadIdMarker" :: NullOrUndefined (UploadIdMarker), "NextKeyMarker" :: NullOrUndefined (NextKeyMarker), "Prefix" :: NullOrUndefined (Prefix), "Delimiter" :: NullOrUndefined (Delimiter), "NextUploadIdMarker" :: NullOrUndefined (NextUploadIdMarker), "MaxUploads" :: NullOrUndefined (MaxUploads), "IsTruncated" :: NullOrUndefined (IsTruncated), "Uploads" :: NullOrUndefined (MultipartUploadList), "CommonPrefixes" :: NullOrUndefined (CommonPrefixList), "EncodingType" :: NullOrUndefined (EncodingType) } -> { "Bucket" :: NullOrUndefined (BucketName), "KeyMarker" :: NullOrUndefined (KeyMarker), "UploadIdMarker" :: NullOrUndefined (UploadIdMarker), "NextKeyMarker" :: NullOrUndefined (NextKeyMarker), "Prefix" :: NullOrUndefined (Prefix), "Delimiter" :: NullOrUndefined (Delimiter), "NextUploadIdMarker" :: NullOrUndefined (NextUploadIdMarker), "MaxUploads" :: NullOrUndefined (MaxUploads), "IsTruncated" :: NullOrUndefined (IsTruncated), "Uploads" :: NullOrUndefined (MultipartUploadList), "CommonPrefixes" :: NullOrUndefined (CommonPrefixList), "EncodingType" :: NullOrUndefined (EncodingType) }) -> ListMultipartUploadsOutput
```

Constructs ListMultipartUploadsOutput's fields from required parameters

#### `ListMultipartUploadsRequest`

``` purescript
newtype ListMultipartUploadsRequest
  = ListMultipartUploadsRequest { "Bucket" :: BucketName, "Delimiter" :: NullOrUndefined (Delimiter), "EncodingType" :: NullOrUndefined (EncodingType), "KeyMarker" :: NullOrUndefined (KeyMarker), "MaxUploads" :: NullOrUndefined (MaxUploads), "Prefix" :: NullOrUndefined (Prefix), "UploadIdMarker" :: NullOrUndefined (UploadIdMarker) }
```

##### Instances
``` purescript
Newtype ListMultipartUploadsRequest _
Generic ListMultipartUploadsRequest _
Show ListMultipartUploadsRequest
Decode ListMultipartUploadsRequest
Encode ListMultipartUploadsRequest
```

#### `newListMultipartUploadsRequest`

``` purescript
newListMultipartUploadsRequest :: BucketName -> ListMultipartUploadsRequest
```

Constructs ListMultipartUploadsRequest from required parameters

#### `newListMultipartUploadsRequest'`

``` purescript
newListMultipartUploadsRequest' :: BucketName -> ({ "Bucket" :: BucketName, "Delimiter" :: NullOrUndefined (Delimiter), "EncodingType" :: NullOrUndefined (EncodingType), "KeyMarker" :: NullOrUndefined (KeyMarker), "MaxUploads" :: NullOrUndefined (MaxUploads), "Prefix" :: NullOrUndefined (Prefix), "UploadIdMarker" :: NullOrUndefined (UploadIdMarker) } -> { "Bucket" :: BucketName, "Delimiter" :: NullOrUndefined (Delimiter), "EncodingType" :: NullOrUndefined (EncodingType), "KeyMarker" :: NullOrUndefined (KeyMarker), "MaxUploads" :: NullOrUndefined (MaxUploads), "Prefix" :: NullOrUndefined (Prefix), "UploadIdMarker" :: NullOrUndefined (UploadIdMarker) }) -> ListMultipartUploadsRequest
```

Constructs ListMultipartUploadsRequest's fields from required parameters

#### `ListObjectVersionsOutput`

``` purescript
newtype ListObjectVersionsOutput
  = ListObjectVersionsOutput { "IsTruncated" :: NullOrUndefined (IsTruncated), "KeyMarker" :: NullOrUndefined (KeyMarker), "VersionIdMarker" :: NullOrUndefined (VersionIdMarker), "NextKeyMarker" :: NullOrUndefined (NextKeyMarker), "NextVersionIdMarker" :: NullOrUndefined (NextVersionIdMarker), "Versions" :: NullOrUndefined (ObjectVersionList), "DeleteMarkers" :: NullOrUndefined (DeleteMarkers), "Name" :: NullOrUndefined (BucketName), "Prefix" :: NullOrUndefined (Prefix), "Delimiter" :: NullOrUndefined (Delimiter), "MaxKeys" :: NullOrUndefined (MaxKeys), "CommonPrefixes" :: NullOrUndefined (CommonPrefixList), "EncodingType" :: NullOrUndefined (EncodingType) }
```

##### Instances
``` purescript
Newtype ListObjectVersionsOutput _
Generic ListObjectVersionsOutput _
Show ListObjectVersionsOutput
Decode ListObjectVersionsOutput
Encode ListObjectVersionsOutput
```

#### `newListObjectVersionsOutput`

``` purescript
newListObjectVersionsOutput :: ListObjectVersionsOutput
```

Constructs ListObjectVersionsOutput from required parameters

#### `newListObjectVersionsOutput'`

``` purescript
newListObjectVersionsOutput' :: ({ "IsTruncated" :: NullOrUndefined (IsTruncated), "KeyMarker" :: NullOrUndefined (KeyMarker), "VersionIdMarker" :: NullOrUndefined (VersionIdMarker), "NextKeyMarker" :: NullOrUndefined (NextKeyMarker), "NextVersionIdMarker" :: NullOrUndefined (NextVersionIdMarker), "Versions" :: NullOrUndefined (ObjectVersionList), "DeleteMarkers" :: NullOrUndefined (DeleteMarkers), "Name" :: NullOrUndefined (BucketName), "Prefix" :: NullOrUndefined (Prefix), "Delimiter" :: NullOrUndefined (Delimiter), "MaxKeys" :: NullOrUndefined (MaxKeys), "CommonPrefixes" :: NullOrUndefined (CommonPrefixList), "EncodingType" :: NullOrUndefined (EncodingType) } -> { "IsTruncated" :: NullOrUndefined (IsTruncated), "KeyMarker" :: NullOrUndefined (KeyMarker), "VersionIdMarker" :: NullOrUndefined (VersionIdMarker), "NextKeyMarker" :: NullOrUndefined (NextKeyMarker), "NextVersionIdMarker" :: NullOrUndefined (NextVersionIdMarker), "Versions" :: NullOrUndefined (ObjectVersionList), "DeleteMarkers" :: NullOrUndefined (DeleteMarkers), "Name" :: NullOrUndefined (BucketName), "Prefix" :: NullOrUndefined (Prefix), "Delimiter" :: NullOrUndefined (Delimiter), "MaxKeys" :: NullOrUndefined (MaxKeys), "CommonPrefixes" :: NullOrUndefined (CommonPrefixList), "EncodingType" :: NullOrUndefined (EncodingType) }) -> ListObjectVersionsOutput
```

Constructs ListObjectVersionsOutput's fields from required parameters

#### `ListObjectVersionsRequest`

``` purescript
newtype ListObjectVersionsRequest
  = ListObjectVersionsRequest { "Bucket" :: BucketName, "Delimiter" :: NullOrUndefined (Delimiter), "EncodingType" :: NullOrUndefined (EncodingType), "KeyMarker" :: NullOrUndefined (KeyMarker), "MaxKeys" :: NullOrUndefined (MaxKeys), "Prefix" :: NullOrUndefined (Prefix), "VersionIdMarker" :: NullOrUndefined (VersionIdMarker) }
```

##### Instances
``` purescript
Newtype ListObjectVersionsRequest _
Generic ListObjectVersionsRequest _
Show ListObjectVersionsRequest
Decode ListObjectVersionsRequest
Encode ListObjectVersionsRequest
```

#### `newListObjectVersionsRequest`

``` purescript
newListObjectVersionsRequest :: BucketName -> ListObjectVersionsRequest
```

Constructs ListObjectVersionsRequest from required parameters

#### `newListObjectVersionsRequest'`

``` purescript
newListObjectVersionsRequest' :: BucketName -> ({ "Bucket" :: BucketName, "Delimiter" :: NullOrUndefined (Delimiter), "EncodingType" :: NullOrUndefined (EncodingType), "KeyMarker" :: NullOrUndefined (KeyMarker), "MaxKeys" :: NullOrUndefined (MaxKeys), "Prefix" :: NullOrUndefined (Prefix), "VersionIdMarker" :: NullOrUndefined (VersionIdMarker) } -> { "Bucket" :: BucketName, "Delimiter" :: NullOrUndefined (Delimiter), "EncodingType" :: NullOrUndefined (EncodingType), "KeyMarker" :: NullOrUndefined (KeyMarker), "MaxKeys" :: NullOrUndefined (MaxKeys), "Prefix" :: NullOrUndefined (Prefix), "VersionIdMarker" :: NullOrUndefined (VersionIdMarker) }) -> ListObjectVersionsRequest
```

Constructs ListObjectVersionsRequest's fields from required parameters

#### `ListObjectsOutput`

``` purescript
newtype ListObjectsOutput
  = ListObjectsOutput { "IsTruncated" :: NullOrUndefined (IsTruncated), "Marker" :: NullOrUndefined (Marker), "NextMarker" :: NullOrUndefined (NextMarker), "Contents" :: NullOrUndefined (ObjectList), "Name" :: NullOrUndefined (BucketName), "Prefix" :: NullOrUndefined (Prefix), "Delimiter" :: NullOrUndefined (Delimiter), "MaxKeys" :: NullOrUndefined (MaxKeys), "CommonPrefixes" :: NullOrUndefined (CommonPrefixList), "EncodingType" :: NullOrUndefined (EncodingType) }
```

##### Instances
``` purescript
Newtype ListObjectsOutput _
Generic ListObjectsOutput _
Show ListObjectsOutput
Decode ListObjectsOutput
Encode ListObjectsOutput
```

#### `newListObjectsOutput`

``` purescript
newListObjectsOutput :: ListObjectsOutput
```

Constructs ListObjectsOutput from required parameters

#### `newListObjectsOutput'`

``` purescript
newListObjectsOutput' :: ({ "IsTruncated" :: NullOrUndefined (IsTruncated), "Marker" :: NullOrUndefined (Marker), "NextMarker" :: NullOrUndefined (NextMarker), "Contents" :: NullOrUndefined (ObjectList), "Name" :: NullOrUndefined (BucketName), "Prefix" :: NullOrUndefined (Prefix), "Delimiter" :: NullOrUndefined (Delimiter), "MaxKeys" :: NullOrUndefined (MaxKeys), "CommonPrefixes" :: NullOrUndefined (CommonPrefixList), "EncodingType" :: NullOrUndefined (EncodingType) } -> { "IsTruncated" :: NullOrUndefined (IsTruncated), "Marker" :: NullOrUndefined (Marker), "NextMarker" :: NullOrUndefined (NextMarker), "Contents" :: NullOrUndefined (ObjectList), "Name" :: NullOrUndefined (BucketName), "Prefix" :: NullOrUndefined (Prefix), "Delimiter" :: NullOrUndefined (Delimiter), "MaxKeys" :: NullOrUndefined (MaxKeys), "CommonPrefixes" :: NullOrUndefined (CommonPrefixList), "EncodingType" :: NullOrUndefined (EncodingType) }) -> ListObjectsOutput
```

Constructs ListObjectsOutput's fields from required parameters

#### `ListObjectsRequest`

``` purescript
newtype ListObjectsRequest
  = ListObjectsRequest { "Bucket" :: BucketName, "Delimiter" :: NullOrUndefined (Delimiter), "EncodingType" :: NullOrUndefined (EncodingType), "Marker" :: NullOrUndefined (Marker), "MaxKeys" :: NullOrUndefined (MaxKeys), "Prefix" :: NullOrUndefined (Prefix), "RequestPayer" :: NullOrUndefined (RequestPayer) }
```

##### Instances
``` purescript
Newtype ListObjectsRequest _
Generic ListObjectsRequest _
Show ListObjectsRequest
Decode ListObjectsRequest
Encode ListObjectsRequest
```

#### `newListObjectsRequest`

``` purescript
newListObjectsRequest :: BucketName -> ListObjectsRequest
```

Constructs ListObjectsRequest from required parameters

#### `newListObjectsRequest'`

``` purescript
newListObjectsRequest' :: BucketName -> ({ "Bucket" :: BucketName, "Delimiter" :: NullOrUndefined (Delimiter), "EncodingType" :: NullOrUndefined (EncodingType), "Marker" :: NullOrUndefined (Marker), "MaxKeys" :: NullOrUndefined (MaxKeys), "Prefix" :: NullOrUndefined (Prefix), "RequestPayer" :: NullOrUndefined (RequestPayer) } -> { "Bucket" :: BucketName, "Delimiter" :: NullOrUndefined (Delimiter), "EncodingType" :: NullOrUndefined (EncodingType), "Marker" :: NullOrUndefined (Marker), "MaxKeys" :: NullOrUndefined (MaxKeys), "Prefix" :: NullOrUndefined (Prefix), "RequestPayer" :: NullOrUndefined (RequestPayer) }) -> ListObjectsRequest
```

Constructs ListObjectsRequest's fields from required parameters

#### `ListObjectsV2Output`

``` purescript
newtype ListObjectsV2Output
  = ListObjectsV2Output { "IsTruncated" :: NullOrUndefined (IsTruncated), "Contents" :: NullOrUndefined (ObjectList), "Name" :: NullOrUndefined (BucketName), "Prefix" :: NullOrUndefined (Prefix), "Delimiter" :: NullOrUndefined (Delimiter), "MaxKeys" :: NullOrUndefined (MaxKeys), "CommonPrefixes" :: NullOrUndefined (CommonPrefixList), "EncodingType" :: NullOrUndefined (EncodingType), "KeyCount" :: NullOrUndefined (KeyCount), "ContinuationToken" :: NullOrUndefined (Token), "NextContinuationToken" :: NullOrUndefined (NextToken), "StartAfter" :: NullOrUndefined (StartAfter) }
```

##### Instances
``` purescript
Newtype ListObjectsV2Output _
Generic ListObjectsV2Output _
Show ListObjectsV2Output
Decode ListObjectsV2Output
Encode ListObjectsV2Output
```

#### `newListObjectsV2Output`

``` purescript
newListObjectsV2Output :: ListObjectsV2Output
```

Constructs ListObjectsV2Output from required parameters

#### `newListObjectsV2Output'`

``` purescript
newListObjectsV2Output' :: ({ "IsTruncated" :: NullOrUndefined (IsTruncated), "Contents" :: NullOrUndefined (ObjectList), "Name" :: NullOrUndefined (BucketName), "Prefix" :: NullOrUndefined (Prefix), "Delimiter" :: NullOrUndefined (Delimiter), "MaxKeys" :: NullOrUndefined (MaxKeys), "CommonPrefixes" :: NullOrUndefined (CommonPrefixList), "EncodingType" :: NullOrUndefined (EncodingType), "KeyCount" :: NullOrUndefined (KeyCount), "ContinuationToken" :: NullOrUndefined (Token), "NextContinuationToken" :: NullOrUndefined (NextToken), "StartAfter" :: NullOrUndefined (StartAfter) } -> { "IsTruncated" :: NullOrUndefined (IsTruncated), "Contents" :: NullOrUndefined (ObjectList), "Name" :: NullOrUndefined (BucketName), "Prefix" :: NullOrUndefined (Prefix), "Delimiter" :: NullOrUndefined (Delimiter), "MaxKeys" :: NullOrUndefined (MaxKeys), "CommonPrefixes" :: NullOrUndefined (CommonPrefixList), "EncodingType" :: NullOrUndefined (EncodingType), "KeyCount" :: NullOrUndefined (KeyCount), "ContinuationToken" :: NullOrUndefined (Token), "NextContinuationToken" :: NullOrUndefined (NextToken), "StartAfter" :: NullOrUndefined (StartAfter) }) -> ListObjectsV2Output
```

Constructs ListObjectsV2Output's fields from required parameters

#### `ListObjectsV2Request`

``` purescript
newtype ListObjectsV2Request
  = ListObjectsV2Request { "Bucket" :: BucketName, "Delimiter" :: NullOrUndefined (Delimiter), "EncodingType" :: NullOrUndefined (EncodingType), "MaxKeys" :: NullOrUndefined (MaxKeys), "Prefix" :: NullOrUndefined (Prefix), "ContinuationToken" :: NullOrUndefined (Token), "FetchOwner" :: NullOrUndefined (FetchOwner), "StartAfter" :: NullOrUndefined (StartAfter), "RequestPayer" :: NullOrUndefined (RequestPayer) }
```

##### Instances
``` purescript
Newtype ListObjectsV2Request _
Generic ListObjectsV2Request _
Show ListObjectsV2Request
Decode ListObjectsV2Request
Encode ListObjectsV2Request
```

#### `newListObjectsV2Request`

``` purescript
newListObjectsV2Request :: BucketName -> ListObjectsV2Request
```

Constructs ListObjectsV2Request from required parameters

#### `newListObjectsV2Request'`

``` purescript
newListObjectsV2Request' :: BucketName -> ({ "Bucket" :: BucketName, "Delimiter" :: NullOrUndefined (Delimiter), "EncodingType" :: NullOrUndefined (EncodingType), "MaxKeys" :: NullOrUndefined (MaxKeys), "Prefix" :: NullOrUndefined (Prefix), "ContinuationToken" :: NullOrUndefined (Token), "FetchOwner" :: NullOrUndefined (FetchOwner), "StartAfter" :: NullOrUndefined (StartAfter), "RequestPayer" :: NullOrUndefined (RequestPayer) } -> { "Bucket" :: BucketName, "Delimiter" :: NullOrUndefined (Delimiter), "EncodingType" :: NullOrUndefined (EncodingType), "MaxKeys" :: NullOrUndefined (MaxKeys), "Prefix" :: NullOrUndefined (Prefix), "ContinuationToken" :: NullOrUndefined (Token), "FetchOwner" :: NullOrUndefined (FetchOwner), "StartAfter" :: NullOrUndefined (StartAfter), "RequestPayer" :: NullOrUndefined (RequestPayer) }) -> ListObjectsV2Request
```

Constructs ListObjectsV2Request's fields from required parameters

#### `ListPartsOutput`

``` purescript
newtype ListPartsOutput
  = ListPartsOutput { "AbortDate" :: NullOrUndefined (AbortDate), "AbortRuleId" :: NullOrUndefined (AbortRuleId), "Bucket" :: NullOrUndefined (BucketName), "Key" :: NullOrUndefined (ObjectKey), "UploadId" :: NullOrUndefined (MultipartUploadId), "PartNumberMarker" :: NullOrUndefined (PartNumberMarker), "NextPartNumberMarker" :: NullOrUndefined (NextPartNumberMarker), "MaxParts" :: NullOrUndefined (MaxParts), "IsTruncated" :: NullOrUndefined (IsTruncated), "Parts" :: NullOrUndefined (Parts), "Initiator" :: NullOrUndefined (Initiator), "Owner" :: NullOrUndefined (Owner), "StorageClass" :: NullOrUndefined (StorageClass), "RequestCharged" :: NullOrUndefined (RequestCharged) }
```

##### Instances
``` purescript
Newtype ListPartsOutput _
Generic ListPartsOutput _
Show ListPartsOutput
Decode ListPartsOutput
Encode ListPartsOutput
```

#### `newListPartsOutput`

``` purescript
newListPartsOutput :: ListPartsOutput
```

Constructs ListPartsOutput from required parameters

#### `newListPartsOutput'`

``` purescript
newListPartsOutput' :: ({ "AbortDate" :: NullOrUndefined (AbortDate), "AbortRuleId" :: NullOrUndefined (AbortRuleId), "Bucket" :: NullOrUndefined (BucketName), "Key" :: NullOrUndefined (ObjectKey), "UploadId" :: NullOrUndefined (MultipartUploadId), "PartNumberMarker" :: NullOrUndefined (PartNumberMarker), "NextPartNumberMarker" :: NullOrUndefined (NextPartNumberMarker), "MaxParts" :: NullOrUndefined (MaxParts), "IsTruncated" :: NullOrUndefined (IsTruncated), "Parts" :: NullOrUndefined (Parts), "Initiator" :: NullOrUndefined (Initiator), "Owner" :: NullOrUndefined (Owner), "StorageClass" :: NullOrUndefined (StorageClass), "RequestCharged" :: NullOrUndefined (RequestCharged) } -> { "AbortDate" :: NullOrUndefined (AbortDate), "AbortRuleId" :: NullOrUndefined (AbortRuleId), "Bucket" :: NullOrUndefined (BucketName), "Key" :: NullOrUndefined (ObjectKey), "UploadId" :: NullOrUndefined (MultipartUploadId), "PartNumberMarker" :: NullOrUndefined (PartNumberMarker), "NextPartNumberMarker" :: NullOrUndefined (NextPartNumberMarker), "MaxParts" :: NullOrUndefined (MaxParts), "IsTruncated" :: NullOrUndefined (IsTruncated), "Parts" :: NullOrUndefined (Parts), "Initiator" :: NullOrUndefined (Initiator), "Owner" :: NullOrUndefined (Owner), "StorageClass" :: NullOrUndefined (StorageClass), "RequestCharged" :: NullOrUndefined (RequestCharged) }) -> ListPartsOutput
```

Constructs ListPartsOutput's fields from required parameters

#### `ListPartsRequest`

``` purescript
newtype ListPartsRequest
  = ListPartsRequest { "Bucket" :: BucketName, "Key" :: ObjectKey, "MaxParts" :: NullOrUndefined (MaxParts), "PartNumberMarker" :: NullOrUndefined (PartNumberMarker), "UploadId" :: MultipartUploadId, "RequestPayer" :: NullOrUndefined (RequestPayer) }
```

##### Instances
``` purescript
Newtype ListPartsRequest _
Generic ListPartsRequest _
Show ListPartsRequest
Decode ListPartsRequest
Encode ListPartsRequest
```

#### `newListPartsRequest`

``` purescript
newListPartsRequest :: BucketName -> ObjectKey -> MultipartUploadId -> ListPartsRequest
```

Constructs ListPartsRequest from required parameters

#### `newListPartsRequest'`

``` purescript
newListPartsRequest' :: BucketName -> ObjectKey -> MultipartUploadId -> ({ "Bucket" :: BucketName, "Key" :: ObjectKey, "MaxParts" :: NullOrUndefined (MaxParts), "PartNumberMarker" :: NullOrUndefined (PartNumberMarker), "UploadId" :: MultipartUploadId, "RequestPayer" :: NullOrUndefined (RequestPayer) } -> { "Bucket" :: BucketName, "Key" :: ObjectKey, "MaxParts" :: NullOrUndefined (MaxParts), "PartNumberMarker" :: NullOrUndefined (PartNumberMarker), "UploadId" :: MultipartUploadId, "RequestPayer" :: NullOrUndefined (RequestPayer) }) -> ListPartsRequest
```

Constructs ListPartsRequest's fields from required parameters

#### `Location`

``` purescript
newtype Location
  = Location String
```

##### Instances
``` purescript
Newtype Location _
Generic Location _
Show Location
Decode Location
Encode Location
```

#### `LocationPrefix`

``` purescript
newtype LocationPrefix
  = LocationPrefix String
```

##### Instances
``` purescript
Newtype LocationPrefix _
Generic LocationPrefix _
Show LocationPrefix
Decode LocationPrefix
Encode LocationPrefix
```

#### `LoggingEnabled`

``` purescript
newtype LoggingEnabled
  = LoggingEnabled { "TargetBucket" :: NullOrUndefined (TargetBucket), "TargetGrants" :: NullOrUndefined (TargetGrants), "TargetPrefix" :: NullOrUndefined (TargetPrefix) }
```

##### Instances
``` purescript
Newtype LoggingEnabled _
Generic LoggingEnabled _
Show LoggingEnabled
Decode LoggingEnabled
Encode LoggingEnabled
```

#### `newLoggingEnabled`

``` purescript
newLoggingEnabled :: LoggingEnabled
```

Constructs LoggingEnabled from required parameters

#### `newLoggingEnabled'`

``` purescript
newLoggingEnabled' :: ({ "TargetBucket" :: NullOrUndefined (TargetBucket), "TargetGrants" :: NullOrUndefined (TargetGrants), "TargetPrefix" :: NullOrUndefined (TargetPrefix) } -> { "TargetBucket" :: NullOrUndefined (TargetBucket), "TargetGrants" :: NullOrUndefined (TargetGrants), "TargetPrefix" :: NullOrUndefined (TargetPrefix) }) -> LoggingEnabled
```

Constructs LoggingEnabled's fields from required parameters

#### `MFA`

``` purescript
newtype MFA
  = MFA String
```

##### Instances
``` purescript
Newtype MFA _
Generic MFA _
Show MFA
Decode MFA
Encode MFA
```

#### `MFADelete`

``` purescript
newtype MFADelete
  = MFADelete String
```

##### Instances
``` purescript
Newtype MFADelete _
Generic MFADelete _
Show MFADelete
Decode MFADelete
Encode MFADelete
```

#### `MFADeleteStatus`

``` purescript
newtype MFADeleteStatus
  = MFADeleteStatus String
```

##### Instances
``` purescript
Newtype MFADeleteStatus _
Generic MFADeleteStatus _
Show MFADeleteStatus
Decode MFADeleteStatus
Encode MFADeleteStatus
```

#### `Marker`

``` purescript
newtype Marker
  = Marker String
```

##### Instances
``` purescript
Newtype Marker _
Generic Marker _
Show Marker
Decode Marker
Encode Marker
```

#### `MaxAgeSeconds`

``` purescript
newtype MaxAgeSeconds
  = MaxAgeSeconds Int
```

##### Instances
``` purescript
Newtype MaxAgeSeconds _
Generic MaxAgeSeconds _
Show MaxAgeSeconds
Decode MaxAgeSeconds
Encode MaxAgeSeconds
```

#### `MaxKeys`

``` purescript
newtype MaxKeys
  = MaxKeys Int
```

##### Instances
``` purescript
Newtype MaxKeys _
Generic MaxKeys _
Show MaxKeys
Decode MaxKeys
Encode MaxKeys
```

#### `MaxParts`

``` purescript
newtype MaxParts
  = MaxParts Int
```

##### Instances
``` purescript
Newtype MaxParts _
Generic MaxParts _
Show MaxParts
Decode MaxParts
Encode MaxParts
```

#### `MaxUploads`

``` purescript
newtype MaxUploads
  = MaxUploads Int
```

##### Instances
``` purescript
Newtype MaxUploads _
Generic MaxUploads _
Show MaxUploads
Decode MaxUploads
Encode MaxUploads
```

#### `Message`

``` purescript
newtype Message
  = Message String
```

##### Instances
``` purescript
Newtype Message _
Generic Message _
Show Message
Decode Message
Encode Message
```

#### `Metadata`

``` purescript
newtype Metadata
  = Metadata (StrMap MetadataValue)
```

##### Instances
``` purescript
Newtype Metadata _
Generic Metadata _
Show Metadata
Decode Metadata
Encode Metadata
```

#### `MetadataDirective`

``` purescript
newtype MetadataDirective
  = MetadataDirective String
```

##### Instances
``` purescript
Newtype MetadataDirective _
Generic MetadataDirective _
Show MetadataDirective
Decode MetadataDirective
Encode MetadataDirective
```

#### `MetadataEntry`

``` purescript
newtype MetadataEntry
  = MetadataEntry { "Name" :: NullOrUndefined (MetadataKey), "Value" :: NullOrUndefined (MetadataValue) }
```

A metadata key-value pair to store with an object.

##### Instances
``` purescript
Newtype MetadataEntry _
Generic MetadataEntry _
Show MetadataEntry
Decode MetadataEntry
Encode MetadataEntry
```

#### `newMetadataEntry`

``` purescript
newMetadataEntry :: MetadataEntry
```

Constructs MetadataEntry from required parameters

#### `newMetadataEntry'`

``` purescript
newMetadataEntry' :: ({ "Name" :: NullOrUndefined (MetadataKey), "Value" :: NullOrUndefined (MetadataValue) } -> { "Name" :: NullOrUndefined (MetadataKey), "Value" :: NullOrUndefined (MetadataValue) }) -> MetadataEntry
```

Constructs MetadataEntry's fields from required parameters

#### `MetadataKey`

``` purescript
newtype MetadataKey
  = MetadataKey String
```

##### Instances
``` purescript
Newtype MetadataKey _
Generic MetadataKey _
Show MetadataKey
Decode MetadataKey
Encode MetadataKey
```

#### `MetadataValue`

``` purescript
newtype MetadataValue
  = MetadataValue String
```

##### Instances
``` purescript
Newtype MetadataValue _
Generic MetadataValue _
Show MetadataValue
Decode MetadataValue
Encode MetadataValue
```

#### `MetricsAndOperator`

``` purescript
newtype MetricsAndOperator
  = MetricsAndOperator { "Prefix" :: NullOrUndefined (Prefix), "Tags" :: NullOrUndefined (TagSet) }
```

##### Instances
``` purescript
Newtype MetricsAndOperator _
Generic MetricsAndOperator _
Show MetricsAndOperator
Decode MetricsAndOperator
Encode MetricsAndOperator
```

#### `newMetricsAndOperator`

``` purescript
newMetricsAndOperator :: MetricsAndOperator
```

Constructs MetricsAndOperator from required parameters

#### `newMetricsAndOperator'`

``` purescript
newMetricsAndOperator' :: ({ "Prefix" :: NullOrUndefined (Prefix), "Tags" :: NullOrUndefined (TagSet) } -> { "Prefix" :: NullOrUndefined (Prefix), "Tags" :: NullOrUndefined (TagSet) }) -> MetricsAndOperator
```

Constructs MetricsAndOperator's fields from required parameters

#### `MetricsConfiguration`

``` purescript
newtype MetricsConfiguration
  = MetricsConfiguration { "Id" :: MetricsId, "Filter" :: NullOrUndefined (MetricsFilter) }
```

##### Instances
``` purescript
Newtype MetricsConfiguration _
Generic MetricsConfiguration _
Show MetricsConfiguration
Decode MetricsConfiguration
Encode MetricsConfiguration
```

#### `newMetricsConfiguration`

``` purescript
newMetricsConfiguration :: MetricsId -> MetricsConfiguration
```

Constructs MetricsConfiguration from required parameters

#### `newMetricsConfiguration'`

``` purescript
newMetricsConfiguration' :: MetricsId -> ({ "Id" :: MetricsId, "Filter" :: NullOrUndefined (MetricsFilter) } -> { "Id" :: MetricsId, "Filter" :: NullOrUndefined (MetricsFilter) }) -> MetricsConfiguration
```

Constructs MetricsConfiguration's fields from required parameters

#### `MetricsConfigurationList`

``` purescript
newtype MetricsConfigurationList
  = MetricsConfigurationList (Array MetricsConfiguration)
```

##### Instances
``` purescript
Newtype MetricsConfigurationList _
Generic MetricsConfigurationList _
Show MetricsConfigurationList
Decode MetricsConfigurationList
Encode MetricsConfigurationList
```

#### `MetricsFilter`

``` purescript
newtype MetricsFilter
  = MetricsFilter { "Prefix" :: NullOrUndefined (Prefix), "Tag" :: NullOrUndefined (Tag), "And" :: NullOrUndefined (MetricsAndOperator) }
```

##### Instances
``` purescript
Newtype MetricsFilter _
Generic MetricsFilter _
Show MetricsFilter
Decode MetricsFilter
Encode MetricsFilter
```

#### `newMetricsFilter`

``` purescript
newMetricsFilter :: MetricsFilter
```

Constructs MetricsFilter from required parameters

#### `newMetricsFilter'`

``` purescript
newMetricsFilter' :: ({ "Prefix" :: NullOrUndefined (Prefix), "Tag" :: NullOrUndefined (Tag), "And" :: NullOrUndefined (MetricsAndOperator) } -> { "Prefix" :: NullOrUndefined (Prefix), "Tag" :: NullOrUndefined (Tag), "And" :: NullOrUndefined (MetricsAndOperator) }) -> MetricsFilter
```

Constructs MetricsFilter's fields from required parameters

#### `MetricsId`

``` purescript
newtype MetricsId
  = MetricsId String
```

##### Instances
``` purescript
Newtype MetricsId _
Generic MetricsId _
Show MetricsId
Decode MetricsId
Encode MetricsId
```

#### `MissingMeta`

``` purescript
newtype MissingMeta
  = MissingMeta Int
```

##### Instances
``` purescript
Newtype MissingMeta _
Generic MissingMeta _
Show MissingMeta
Decode MissingMeta
Encode MissingMeta
```

#### `MultipartUpload`

``` purescript
newtype MultipartUpload
  = MultipartUpload { "UploadId" :: NullOrUndefined (MultipartUploadId), "Key" :: NullOrUndefined (ObjectKey), "Initiated" :: NullOrUndefined (Initiated), "StorageClass" :: NullOrUndefined (StorageClass), "Owner" :: NullOrUndefined (Owner), "Initiator" :: NullOrUndefined (Initiator) }
```

##### Instances
``` purescript
Newtype MultipartUpload _
Generic MultipartUpload _
Show MultipartUpload
Decode MultipartUpload
Encode MultipartUpload
```

#### `newMultipartUpload`

``` purescript
newMultipartUpload :: MultipartUpload
```

Constructs MultipartUpload from required parameters

#### `newMultipartUpload'`

``` purescript
newMultipartUpload' :: ({ "UploadId" :: NullOrUndefined (MultipartUploadId), "Key" :: NullOrUndefined (ObjectKey), "Initiated" :: NullOrUndefined (Initiated), "StorageClass" :: NullOrUndefined (StorageClass), "Owner" :: NullOrUndefined (Owner), "Initiator" :: NullOrUndefined (Initiator) } -> { "UploadId" :: NullOrUndefined (MultipartUploadId), "Key" :: NullOrUndefined (ObjectKey), "Initiated" :: NullOrUndefined (Initiated), "StorageClass" :: NullOrUndefined (StorageClass), "Owner" :: NullOrUndefined (Owner), "Initiator" :: NullOrUndefined (Initiator) }) -> MultipartUpload
```

Constructs MultipartUpload's fields from required parameters

#### `MultipartUploadId`

``` purescript
newtype MultipartUploadId
  = MultipartUploadId String
```

##### Instances
``` purescript
Newtype MultipartUploadId _
Generic MultipartUploadId _
Show MultipartUploadId
Decode MultipartUploadId
Encode MultipartUploadId
```

#### `MultipartUploadList`

``` purescript
newtype MultipartUploadList
  = MultipartUploadList (Array MultipartUpload)
```

##### Instances
``` purescript
Newtype MultipartUploadList _
Generic MultipartUploadList _
Show MultipartUploadList
Decode MultipartUploadList
Encode MultipartUploadList
```

#### `NextKeyMarker`

``` purescript
newtype NextKeyMarker
  = NextKeyMarker String
```

##### Instances
``` purescript
Newtype NextKeyMarker _
Generic NextKeyMarker _
Show NextKeyMarker
Decode NextKeyMarker
Encode NextKeyMarker
```

#### `NextMarker`

``` purescript
newtype NextMarker
  = NextMarker String
```

##### Instances
``` purescript
Newtype NextMarker _
Generic NextMarker _
Show NextMarker
Decode NextMarker
Encode NextMarker
```

#### `NextPartNumberMarker`

``` purescript
newtype NextPartNumberMarker
  = NextPartNumberMarker Int
```

##### Instances
``` purescript
Newtype NextPartNumberMarker _
Generic NextPartNumberMarker _
Show NextPartNumberMarker
Decode NextPartNumberMarker
Encode NextPartNumberMarker
```

#### `NextToken`

``` purescript
newtype NextToken
  = NextToken String
```

##### Instances
``` purescript
Newtype NextToken _
Generic NextToken _
Show NextToken
Decode NextToken
Encode NextToken
```

#### `NextUploadIdMarker`

``` purescript
newtype NextUploadIdMarker
  = NextUploadIdMarker String
```

##### Instances
``` purescript
Newtype NextUploadIdMarker _
Generic NextUploadIdMarker _
Show NextUploadIdMarker
Decode NextUploadIdMarker
Encode NextUploadIdMarker
```

#### `NextVersionIdMarker`

``` purescript
newtype NextVersionIdMarker
  = NextVersionIdMarker String
```

##### Instances
``` purescript
Newtype NextVersionIdMarker _
Generic NextVersionIdMarker _
Show NextVersionIdMarker
Decode NextVersionIdMarker
Encode NextVersionIdMarker
```

#### `NoSuchBucket`

``` purescript
newtype NoSuchBucket
  = NoSuchBucket NoArguments
```

The specified bucket does not exist.

##### Instances
``` purescript
Newtype NoSuchBucket _
Generic NoSuchBucket _
Show NoSuchBucket
Decode NoSuchBucket
Encode NoSuchBucket
```

#### `NoSuchKey`

``` purescript
newtype NoSuchKey
  = NoSuchKey NoArguments
```

The specified key does not exist.

##### Instances
``` purescript
Newtype NoSuchKey _
Generic NoSuchKey _
Show NoSuchKey
Decode NoSuchKey
Encode NoSuchKey
```

#### `NoSuchUpload`

``` purescript
newtype NoSuchUpload
  = NoSuchUpload NoArguments
```

The specified multipart upload does not exist.

##### Instances
``` purescript
Newtype NoSuchUpload _
Generic NoSuchUpload _
Show NoSuchUpload
Decode NoSuchUpload
Encode NoSuchUpload
```

#### `NoncurrentVersionExpiration`

``` purescript
newtype NoncurrentVersionExpiration
  = NoncurrentVersionExpiration { "NoncurrentDays" :: NullOrUndefined (Days) }
```

Specifies when noncurrent object versions expire. Upon expiration, Amazon S3 permanently deletes the noncurrent object versions. You set this lifecycle configuration action on a bucket that has versioning enabled (or suspended) to request that Amazon S3 delete noncurrent object versions at a specific period in the object's lifetime.

##### Instances
``` purescript
Newtype NoncurrentVersionExpiration _
Generic NoncurrentVersionExpiration _
Show NoncurrentVersionExpiration
Decode NoncurrentVersionExpiration
Encode NoncurrentVersionExpiration
```

#### `newNoncurrentVersionExpiration`

``` purescript
newNoncurrentVersionExpiration :: NoncurrentVersionExpiration
```

Constructs NoncurrentVersionExpiration from required parameters

#### `newNoncurrentVersionExpiration'`

``` purescript
newNoncurrentVersionExpiration' :: ({ "NoncurrentDays" :: NullOrUndefined (Days) } -> { "NoncurrentDays" :: NullOrUndefined (Days) }) -> NoncurrentVersionExpiration
```

Constructs NoncurrentVersionExpiration's fields from required parameters

#### `NoncurrentVersionTransition`

``` purescript
newtype NoncurrentVersionTransition
  = NoncurrentVersionTransition { "NoncurrentDays" :: NullOrUndefined (Days), "StorageClass" :: NullOrUndefined (TransitionStorageClass) }
```

Container for the transition rule that describes when noncurrent objects transition to the STANDARD_IA or GLACIER storage class. If your bucket is versioning-enabled (or versioning is suspended), you can set this action to request that Amazon S3 transition noncurrent object versions to the STANDARD_IA or GLACIER storage class at a specific period in the object's lifetime.

##### Instances
``` purescript
Newtype NoncurrentVersionTransition _
Generic NoncurrentVersionTransition _
Show NoncurrentVersionTransition
Decode NoncurrentVersionTransition
Encode NoncurrentVersionTransition
```

#### `newNoncurrentVersionTransition`

``` purescript
newNoncurrentVersionTransition :: NoncurrentVersionTransition
```

Constructs NoncurrentVersionTransition from required parameters

#### `newNoncurrentVersionTransition'`

``` purescript
newNoncurrentVersionTransition' :: ({ "NoncurrentDays" :: NullOrUndefined (Days), "StorageClass" :: NullOrUndefined (TransitionStorageClass) } -> { "NoncurrentDays" :: NullOrUndefined (Days), "StorageClass" :: NullOrUndefined (TransitionStorageClass) }) -> NoncurrentVersionTransition
```

Constructs NoncurrentVersionTransition's fields from required parameters

#### `NoncurrentVersionTransitionList`

``` purescript
newtype NoncurrentVersionTransitionList
  = NoncurrentVersionTransitionList (Array NoncurrentVersionTransition)
```

##### Instances
``` purescript
Newtype NoncurrentVersionTransitionList _
Generic NoncurrentVersionTransitionList _
Show NoncurrentVersionTransitionList
Decode NoncurrentVersionTransitionList
Encode NoncurrentVersionTransitionList
```

#### `NotificationConfiguration`

``` purescript
newtype NotificationConfiguration
  = NotificationConfiguration { "TopicConfigurations" :: NullOrUndefined (TopicConfigurationList), "QueueConfigurations" :: NullOrUndefined (QueueConfigurationList), "LambdaFunctionConfigurations" :: NullOrUndefined (LambdaFunctionConfigurationList) }
```

Container for specifying the notification configuration of the bucket. If this element is empty, notifications are turned off on the bucket.

##### Instances
``` purescript
Newtype NotificationConfiguration _
Generic NotificationConfiguration _
Show NotificationConfiguration
Decode NotificationConfiguration
Encode NotificationConfiguration
```

#### `newNotificationConfiguration`

``` purescript
newNotificationConfiguration :: NotificationConfiguration
```

Constructs NotificationConfiguration from required parameters

#### `newNotificationConfiguration'`

``` purescript
newNotificationConfiguration' :: ({ "TopicConfigurations" :: NullOrUndefined (TopicConfigurationList), "QueueConfigurations" :: NullOrUndefined (QueueConfigurationList), "LambdaFunctionConfigurations" :: NullOrUndefined (LambdaFunctionConfigurationList) } -> { "TopicConfigurations" :: NullOrUndefined (TopicConfigurationList), "QueueConfigurations" :: NullOrUndefined (QueueConfigurationList), "LambdaFunctionConfigurations" :: NullOrUndefined (LambdaFunctionConfigurationList) }) -> NotificationConfiguration
```

Constructs NotificationConfiguration's fields from required parameters

#### `NotificationConfigurationDeprecated`

``` purescript
newtype NotificationConfigurationDeprecated
  = NotificationConfigurationDeprecated { "TopicConfiguration" :: NullOrUndefined (TopicConfigurationDeprecated), "QueueConfiguration" :: NullOrUndefined (QueueConfigurationDeprecated), "CloudFunctionConfiguration" :: NullOrUndefined (CloudFunctionConfiguration) }
```

##### Instances
``` purescript
Newtype NotificationConfigurationDeprecated _
Generic NotificationConfigurationDeprecated _
Show NotificationConfigurationDeprecated
Decode NotificationConfigurationDeprecated
Encode NotificationConfigurationDeprecated
```

#### `newNotificationConfigurationDeprecated`

``` purescript
newNotificationConfigurationDeprecated :: NotificationConfigurationDeprecated
```

Constructs NotificationConfigurationDeprecated from required parameters

#### `newNotificationConfigurationDeprecated'`

``` purescript
newNotificationConfigurationDeprecated' :: ({ "TopicConfiguration" :: NullOrUndefined (TopicConfigurationDeprecated), "QueueConfiguration" :: NullOrUndefined (QueueConfigurationDeprecated), "CloudFunctionConfiguration" :: NullOrUndefined (CloudFunctionConfiguration) } -> { "TopicConfiguration" :: NullOrUndefined (TopicConfigurationDeprecated), "QueueConfiguration" :: NullOrUndefined (QueueConfigurationDeprecated), "CloudFunctionConfiguration" :: NullOrUndefined (CloudFunctionConfiguration) }) -> NotificationConfigurationDeprecated
```

Constructs NotificationConfigurationDeprecated's fields from required parameters

#### `NotificationConfigurationFilter`

``` purescript
newtype NotificationConfigurationFilter
  = NotificationConfigurationFilter { "Key" :: NullOrUndefined (S3KeyFilter) }
```

Container for object key name filtering rules. For information about key name filtering, go to <a href="http://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html">Configuring Event Notifications</a> in the Amazon Simple Storage Service Developer Guide.

##### Instances
``` purescript
Newtype NotificationConfigurationFilter _
Generic NotificationConfigurationFilter _
Show NotificationConfigurationFilter
Decode NotificationConfigurationFilter
Encode NotificationConfigurationFilter
```

#### `newNotificationConfigurationFilter`

``` purescript
newNotificationConfigurationFilter :: NotificationConfigurationFilter
```

Constructs NotificationConfigurationFilter from required parameters

#### `newNotificationConfigurationFilter'`

``` purescript
newNotificationConfigurationFilter' :: ({ "Key" :: NullOrUndefined (S3KeyFilter) } -> { "Key" :: NullOrUndefined (S3KeyFilter) }) -> NotificationConfigurationFilter
```

Constructs NotificationConfigurationFilter's fields from required parameters

#### `NotificationId`

``` purescript
newtype NotificationId
  = NotificationId String
```

Optional unique identifier for configurations in a notification configuration. If you don't provide one, Amazon S3 will assign an ID.

##### Instances
``` purescript
Newtype NotificationId _
Generic NotificationId _
Show NotificationId
Decode NotificationId
Encode NotificationId
```

#### `Object`

``` purescript
newtype Object
  = Object { "Key" :: NullOrUndefined (ObjectKey), "LastModified" :: NullOrUndefined (LastModified), "ETag" :: NullOrUndefined (ETag), "Size" :: NullOrUndefined (Size), "StorageClass" :: NullOrUndefined (ObjectStorageClass), "Owner" :: NullOrUndefined (Owner) }
```

##### Instances
``` purescript
Newtype Object _
Generic Object _
Show Object
Decode Object
Encode Object
```

#### `newObject`

``` purescript
newObject :: Object
```

Constructs Object from required parameters

#### `newObject'`

``` purescript
newObject' :: ({ "Key" :: NullOrUndefined (ObjectKey), "LastModified" :: NullOrUndefined (LastModified), "ETag" :: NullOrUndefined (ETag), "Size" :: NullOrUndefined (Size), "StorageClass" :: NullOrUndefined (ObjectStorageClass), "Owner" :: NullOrUndefined (Owner) } -> { "Key" :: NullOrUndefined (ObjectKey), "LastModified" :: NullOrUndefined (LastModified), "ETag" :: NullOrUndefined (ETag), "Size" :: NullOrUndefined (Size), "StorageClass" :: NullOrUndefined (ObjectStorageClass), "Owner" :: NullOrUndefined (Owner) }) -> Object
```

Constructs Object's fields from required parameters

#### `ObjectAlreadyInActiveTierError`

``` purescript
newtype ObjectAlreadyInActiveTierError
  = ObjectAlreadyInActiveTierError NoArguments
```

This operation is not allowed against this storage tier

##### Instances
``` purescript
Newtype ObjectAlreadyInActiveTierError _
Generic ObjectAlreadyInActiveTierError _
Show ObjectAlreadyInActiveTierError
Decode ObjectAlreadyInActiveTierError
Encode ObjectAlreadyInActiveTierError
```

#### `ObjectCannedACL`

``` purescript
newtype ObjectCannedACL
  = ObjectCannedACL String
```

##### Instances
``` purescript
Newtype ObjectCannedACL _
Generic ObjectCannedACL _
Show ObjectCannedACL
Decode ObjectCannedACL
Encode ObjectCannedACL
```

#### `ObjectIdentifier`

``` purescript
newtype ObjectIdentifier
  = ObjectIdentifier { "Key" :: ObjectKey, "VersionId" :: NullOrUndefined (ObjectVersionId) }
```

##### Instances
``` purescript
Newtype ObjectIdentifier _
Generic ObjectIdentifier _
Show ObjectIdentifier
Decode ObjectIdentifier
Encode ObjectIdentifier
```

#### `newObjectIdentifier`

``` purescript
newObjectIdentifier :: ObjectKey -> ObjectIdentifier
```

Constructs ObjectIdentifier from required parameters

#### `newObjectIdentifier'`

``` purescript
newObjectIdentifier' :: ObjectKey -> ({ "Key" :: ObjectKey, "VersionId" :: NullOrUndefined (ObjectVersionId) } -> { "Key" :: ObjectKey, "VersionId" :: NullOrUndefined (ObjectVersionId) }) -> ObjectIdentifier
```

Constructs ObjectIdentifier's fields from required parameters

#### `ObjectIdentifierList`

``` purescript
newtype ObjectIdentifierList
  = ObjectIdentifierList (Array ObjectIdentifier)
```

##### Instances
``` purescript
Newtype ObjectIdentifierList _
Generic ObjectIdentifierList _
Show ObjectIdentifierList
Decode ObjectIdentifierList
Encode ObjectIdentifierList
```

#### `ObjectKey`

``` purescript
newtype ObjectKey
  = ObjectKey String
```

##### Instances
``` purescript
Newtype ObjectKey _
Generic ObjectKey _
Show ObjectKey
Decode ObjectKey
Encode ObjectKey
```

#### `ObjectList`

``` purescript
newtype ObjectList
  = ObjectList (Array Object)
```

##### Instances
``` purescript
Newtype ObjectList _
Generic ObjectList _
Show ObjectList
Decode ObjectList
Encode ObjectList
```

#### `ObjectNotInActiveTierError`

``` purescript
newtype ObjectNotInActiveTierError
  = ObjectNotInActiveTierError NoArguments
```

The source object of the COPY operation is not in the active tier and is only stored in Amazon Glacier.

##### Instances
``` purescript
Newtype ObjectNotInActiveTierError _
Generic ObjectNotInActiveTierError _
Show ObjectNotInActiveTierError
Decode ObjectNotInActiveTierError
Encode ObjectNotInActiveTierError
```

#### `ObjectStorageClass`

``` purescript
newtype ObjectStorageClass
  = ObjectStorageClass String
```

##### Instances
``` purescript
Newtype ObjectStorageClass _
Generic ObjectStorageClass _
Show ObjectStorageClass
Decode ObjectStorageClass
Encode ObjectStorageClass
```

#### `ObjectVersion`

``` purescript
newtype ObjectVersion
  = ObjectVersion { "ETag" :: NullOrUndefined (ETag), "Size" :: NullOrUndefined (Size), "StorageClass" :: NullOrUndefined (ObjectVersionStorageClass), "Key" :: NullOrUndefined (ObjectKey), "VersionId" :: NullOrUndefined (ObjectVersionId), "IsLatest" :: NullOrUndefined (IsLatest), "LastModified" :: NullOrUndefined (LastModified), "Owner" :: NullOrUndefined (Owner) }
```

##### Instances
``` purescript
Newtype ObjectVersion _
Generic ObjectVersion _
Show ObjectVersion
Decode ObjectVersion
Encode ObjectVersion
```

#### `newObjectVersion`

``` purescript
newObjectVersion :: ObjectVersion
```

Constructs ObjectVersion from required parameters

#### `newObjectVersion'`

``` purescript
newObjectVersion' :: ({ "ETag" :: NullOrUndefined (ETag), "Size" :: NullOrUndefined (Size), "StorageClass" :: NullOrUndefined (ObjectVersionStorageClass), "Key" :: NullOrUndefined (ObjectKey), "VersionId" :: NullOrUndefined (ObjectVersionId), "IsLatest" :: NullOrUndefined (IsLatest), "LastModified" :: NullOrUndefined (LastModified), "Owner" :: NullOrUndefined (Owner) } -> { "ETag" :: NullOrUndefined (ETag), "Size" :: NullOrUndefined (Size), "StorageClass" :: NullOrUndefined (ObjectVersionStorageClass), "Key" :: NullOrUndefined (ObjectKey), "VersionId" :: NullOrUndefined (ObjectVersionId), "IsLatest" :: NullOrUndefined (IsLatest), "LastModified" :: NullOrUndefined (LastModified), "Owner" :: NullOrUndefined (Owner) }) -> ObjectVersion
```

Constructs ObjectVersion's fields from required parameters

#### `ObjectVersionId`

``` purescript
newtype ObjectVersionId
  = ObjectVersionId String
```

##### Instances
``` purescript
Newtype ObjectVersionId _
Generic ObjectVersionId _
Show ObjectVersionId
Decode ObjectVersionId
Encode ObjectVersionId
```

#### `ObjectVersionList`

``` purescript
newtype ObjectVersionList
  = ObjectVersionList (Array ObjectVersion)
```

##### Instances
``` purescript
Newtype ObjectVersionList _
Generic ObjectVersionList _
Show ObjectVersionList
Decode ObjectVersionList
Encode ObjectVersionList
```

#### `ObjectVersionStorageClass`

``` purescript
newtype ObjectVersionStorageClass
  = ObjectVersionStorageClass String
```

##### Instances
``` purescript
Newtype ObjectVersionStorageClass _
Generic ObjectVersionStorageClass _
Show ObjectVersionStorageClass
Decode ObjectVersionStorageClass
Encode ObjectVersionStorageClass
```

#### `OutputLocation`

``` purescript
newtype OutputLocation
  = OutputLocation { "S3" :: NullOrUndefined (S3Location) }
```

Describes the location where the restore job's output is stored.

##### Instances
``` purescript
Newtype OutputLocation _
Generic OutputLocation _
Show OutputLocation
Decode OutputLocation
Encode OutputLocation
```

#### `newOutputLocation`

``` purescript
newOutputLocation :: OutputLocation
```

Constructs OutputLocation from required parameters

#### `newOutputLocation'`

``` purescript
newOutputLocation' :: ({ "S3" :: NullOrUndefined (S3Location) } -> { "S3" :: NullOrUndefined (S3Location) }) -> OutputLocation
```

Constructs OutputLocation's fields from required parameters

#### `OutputSerialization`

``` purescript
newtype OutputSerialization
  = OutputSerialization { "CSV" :: NullOrUndefined (CSVOutput) }
```

Describes how results of the Select job are serialized.

##### Instances
``` purescript
Newtype OutputSerialization _
Generic OutputSerialization _
Show OutputSerialization
Decode OutputSerialization
Encode OutputSerialization
```

#### `newOutputSerialization`

``` purescript
newOutputSerialization :: OutputSerialization
```

Constructs OutputSerialization from required parameters

#### `newOutputSerialization'`

``` purescript
newOutputSerialization' :: ({ "CSV" :: NullOrUndefined (CSVOutput) } -> { "CSV" :: NullOrUndefined (CSVOutput) }) -> OutputSerialization
```

Constructs OutputSerialization's fields from required parameters

#### `Owner`

``` purescript
newtype Owner
  = Owner { "DisplayName" :: NullOrUndefined (DisplayName), "ID" :: NullOrUndefined (ID) }
```

##### Instances
``` purescript
Newtype Owner _
Generic Owner _
Show Owner
Decode Owner
Encode Owner
```

#### `newOwner`

``` purescript
newOwner :: Owner
```

Constructs Owner from required parameters

#### `newOwner'`

``` purescript
newOwner' :: ({ "DisplayName" :: NullOrUndefined (DisplayName), "ID" :: NullOrUndefined (ID) } -> { "DisplayName" :: NullOrUndefined (DisplayName), "ID" :: NullOrUndefined (ID) }) -> Owner
```

Constructs Owner's fields from required parameters

#### `OwnerOverride`

``` purescript
newtype OwnerOverride
  = OwnerOverride String
```

##### Instances
``` purescript
Newtype OwnerOverride _
Generic OwnerOverride _
Show OwnerOverride
Decode OwnerOverride
Encode OwnerOverride
```

#### `Part`

``` purescript
newtype Part
  = Part { "PartNumber" :: NullOrUndefined (PartNumber), "LastModified" :: NullOrUndefined (LastModified), "ETag" :: NullOrUndefined (ETag), "Size" :: NullOrUndefined (Size) }
```

##### Instances
``` purescript
Newtype Part _
Generic Part _
Show Part
Decode Part
Encode Part
```

#### `newPart`

``` purescript
newPart :: Part
```

Constructs Part from required parameters

#### `newPart'`

``` purescript
newPart' :: ({ "PartNumber" :: NullOrUndefined (PartNumber), "LastModified" :: NullOrUndefined (LastModified), "ETag" :: NullOrUndefined (ETag), "Size" :: NullOrUndefined (Size) } -> { "PartNumber" :: NullOrUndefined (PartNumber), "LastModified" :: NullOrUndefined (LastModified), "ETag" :: NullOrUndefined (ETag), "Size" :: NullOrUndefined (Size) }) -> Part
```

Constructs Part's fields from required parameters

#### `PartNumber`

``` purescript
newtype PartNumber
  = PartNumber Int
```

##### Instances
``` purescript
Newtype PartNumber _
Generic PartNumber _
Show PartNumber
Decode PartNumber
Encode PartNumber
```

#### `PartNumberMarker`

``` purescript
newtype PartNumberMarker
  = PartNumberMarker Int
```

##### Instances
``` purescript
Newtype PartNumberMarker _
Generic PartNumberMarker _
Show PartNumberMarker
Decode PartNumberMarker
Encode PartNumberMarker
```

#### `Parts`

``` purescript
newtype Parts
  = Parts (Array Part)
```

##### Instances
``` purescript
Newtype Parts _
Generic Parts _
Show Parts
Decode Parts
Encode Parts
```

#### `PartsCount`

``` purescript
newtype PartsCount
  = PartsCount Int
```

##### Instances
``` purescript
Newtype PartsCount _
Generic PartsCount _
Show PartsCount
Decode PartsCount
Encode PartsCount
```

#### `Payer`

``` purescript
newtype Payer
  = Payer String
```

##### Instances
``` purescript
Newtype Payer _
Generic Payer _
Show Payer
Decode Payer
Encode Payer
```

#### `Permission`

``` purescript
newtype Permission
  = Permission String
```

##### Instances
``` purescript
Newtype Permission _
Generic Permission _
Show Permission
Decode Permission
Encode Permission
```

#### `Policy`

``` purescript
newtype Policy
  = Policy String
```

##### Instances
``` purescript
Newtype Policy _
Generic Policy _
Show Policy
Decode Policy
Encode Policy
```

#### `Prefix`

``` purescript
newtype Prefix
  = Prefix String
```

##### Instances
``` purescript
Newtype Prefix _
Generic Prefix _
Show Prefix
Decode Prefix
Encode Prefix
```

#### `Protocol`

``` purescript
newtype Protocol
  = Protocol String
```

##### Instances
``` purescript
Newtype Protocol _
Generic Protocol _
Show Protocol
Decode Protocol
Encode Protocol
```

#### `PutBucketAccelerateConfigurationRequest`

``` purescript
newtype PutBucketAccelerateConfigurationRequest
  = PutBucketAccelerateConfigurationRequest { "Bucket" :: BucketName, "AccelerateConfiguration" :: AccelerateConfiguration }
```

##### Instances
``` purescript
Newtype PutBucketAccelerateConfigurationRequest _
Generic PutBucketAccelerateConfigurationRequest _
Show PutBucketAccelerateConfigurationRequest
Decode PutBucketAccelerateConfigurationRequest
Encode PutBucketAccelerateConfigurationRequest
```

#### `newPutBucketAccelerateConfigurationRequest`

``` purescript
newPutBucketAccelerateConfigurationRequest :: AccelerateConfiguration -> BucketName -> PutBucketAccelerateConfigurationRequest
```

Constructs PutBucketAccelerateConfigurationRequest from required parameters

#### `newPutBucketAccelerateConfigurationRequest'`

``` purescript
newPutBucketAccelerateConfigurationRequest' :: AccelerateConfiguration -> BucketName -> ({ "Bucket" :: BucketName, "AccelerateConfiguration" :: AccelerateConfiguration } -> { "Bucket" :: BucketName, "AccelerateConfiguration" :: AccelerateConfiguration }) -> PutBucketAccelerateConfigurationRequest
```

Constructs PutBucketAccelerateConfigurationRequest's fields from required parameters

#### `PutBucketAclRequest`

``` purescript
newtype PutBucketAclRequest
  = PutBucketAclRequest { "ACL" :: NullOrUndefined (BucketCannedACL), "AccessControlPolicy" :: NullOrUndefined (AccessControlPolicy), "Bucket" :: BucketName, "ContentMD5" :: NullOrUndefined (ContentMD5), "GrantFullControl" :: NullOrUndefined (GrantFullControl), "GrantRead" :: NullOrUndefined (GrantRead), "GrantReadACP" :: NullOrUndefined (GrantReadACP), "GrantWrite" :: NullOrUndefined (GrantWrite), "GrantWriteACP" :: NullOrUndefined (GrantWriteACP) }
```

##### Instances
``` purescript
Newtype PutBucketAclRequest _
Generic PutBucketAclRequest _
Show PutBucketAclRequest
Decode PutBucketAclRequest
Encode PutBucketAclRequest
```

#### `newPutBucketAclRequest`

``` purescript
newPutBucketAclRequest :: BucketName -> PutBucketAclRequest
```

Constructs PutBucketAclRequest from required parameters

#### `newPutBucketAclRequest'`

``` purescript
newPutBucketAclRequest' :: BucketName -> ({ "ACL" :: NullOrUndefined (BucketCannedACL), "AccessControlPolicy" :: NullOrUndefined (AccessControlPolicy), "Bucket" :: BucketName, "ContentMD5" :: NullOrUndefined (ContentMD5), "GrantFullControl" :: NullOrUndefined (GrantFullControl), "GrantRead" :: NullOrUndefined (GrantRead), "GrantReadACP" :: NullOrUndefined (GrantReadACP), "GrantWrite" :: NullOrUndefined (GrantWrite), "GrantWriteACP" :: NullOrUndefined (GrantWriteACP) } -> { "ACL" :: NullOrUndefined (BucketCannedACL), "AccessControlPolicy" :: NullOrUndefined (AccessControlPolicy), "Bucket" :: BucketName, "ContentMD5" :: NullOrUndefined (ContentMD5), "GrantFullControl" :: NullOrUndefined (GrantFullControl), "GrantRead" :: NullOrUndefined (GrantRead), "GrantReadACP" :: NullOrUndefined (GrantReadACP), "GrantWrite" :: NullOrUndefined (GrantWrite), "GrantWriteACP" :: NullOrUndefined (GrantWriteACP) }) -> PutBucketAclRequest
```

Constructs PutBucketAclRequest's fields from required parameters

#### `PutBucketAnalyticsConfigurationRequest`

``` purescript
newtype PutBucketAnalyticsConfigurationRequest
  = PutBucketAnalyticsConfigurationRequest { "Bucket" :: BucketName, "Id" :: AnalyticsId, "AnalyticsConfiguration" :: AnalyticsConfiguration }
```

##### Instances
``` purescript
Newtype PutBucketAnalyticsConfigurationRequest _
Generic PutBucketAnalyticsConfigurationRequest _
Show PutBucketAnalyticsConfigurationRequest
Decode PutBucketAnalyticsConfigurationRequest
Encode PutBucketAnalyticsConfigurationRequest
```

#### `newPutBucketAnalyticsConfigurationRequest`

``` purescript
newPutBucketAnalyticsConfigurationRequest :: AnalyticsConfiguration -> BucketName -> AnalyticsId -> PutBucketAnalyticsConfigurationRequest
```

Constructs PutBucketAnalyticsConfigurationRequest from required parameters

#### `newPutBucketAnalyticsConfigurationRequest'`

``` purescript
newPutBucketAnalyticsConfigurationRequest' :: AnalyticsConfiguration -> BucketName -> AnalyticsId -> ({ "Bucket" :: BucketName, "Id" :: AnalyticsId, "AnalyticsConfiguration" :: AnalyticsConfiguration } -> { "Bucket" :: BucketName, "Id" :: AnalyticsId, "AnalyticsConfiguration" :: AnalyticsConfiguration }) -> PutBucketAnalyticsConfigurationRequest
```

Constructs PutBucketAnalyticsConfigurationRequest's fields from required parameters

#### `PutBucketCorsRequest`

``` purescript
newtype PutBucketCorsRequest
  = PutBucketCorsRequest { "Bucket" :: BucketName, "CORSConfiguration" :: CORSConfiguration, "ContentMD5" :: NullOrUndefined (ContentMD5) }
```

##### Instances
``` purescript
Newtype PutBucketCorsRequest _
Generic PutBucketCorsRequest _
Show PutBucketCorsRequest
Decode PutBucketCorsRequest
Encode PutBucketCorsRequest
```

#### `newPutBucketCorsRequest`

``` purescript
newPutBucketCorsRequest :: BucketName -> CORSConfiguration -> PutBucketCorsRequest
```

Constructs PutBucketCorsRequest from required parameters

#### `newPutBucketCorsRequest'`

``` purescript
newPutBucketCorsRequest' :: BucketName -> CORSConfiguration -> ({ "Bucket" :: BucketName, "CORSConfiguration" :: CORSConfiguration, "ContentMD5" :: NullOrUndefined (ContentMD5) } -> { "Bucket" :: BucketName, "CORSConfiguration" :: CORSConfiguration, "ContentMD5" :: NullOrUndefined (ContentMD5) }) -> PutBucketCorsRequest
```

Constructs PutBucketCorsRequest's fields from required parameters

#### `PutBucketEncryptionRequest`

``` purescript
newtype PutBucketEncryptionRequest
  = PutBucketEncryptionRequest { "Bucket" :: BucketName, "ContentMD5" :: NullOrUndefined (ContentMD5), "ServerSideEncryptionConfiguration" :: ServerSideEncryptionConfiguration }
```

##### Instances
``` purescript
Newtype PutBucketEncryptionRequest _
Generic PutBucketEncryptionRequest _
Show PutBucketEncryptionRequest
Decode PutBucketEncryptionRequest
Encode PutBucketEncryptionRequest
```

#### `newPutBucketEncryptionRequest`

``` purescript
newPutBucketEncryptionRequest :: BucketName -> ServerSideEncryptionConfiguration -> PutBucketEncryptionRequest
```

Constructs PutBucketEncryptionRequest from required parameters

#### `newPutBucketEncryptionRequest'`

``` purescript
newPutBucketEncryptionRequest' :: BucketName -> ServerSideEncryptionConfiguration -> ({ "Bucket" :: BucketName, "ContentMD5" :: NullOrUndefined (ContentMD5), "ServerSideEncryptionConfiguration" :: ServerSideEncryptionConfiguration } -> { "Bucket" :: BucketName, "ContentMD5" :: NullOrUndefined (ContentMD5), "ServerSideEncryptionConfiguration" :: ServerSideEncryptionConfiguration }) -> PutBucketEncryptionRequest
```

Constructs PutBucketEncryptionRequest's fields from required parameters

#### `PutBucketInventoryConfigurationRequest`

``` purescript
newtype PutBucketInventoryConfigurationRequest
  = PutBucketInventoryConfigurationRequest { "Bucket" :: BucketName, "Id" :: InventoryId, "InventoryConfiguration" :: InventoryConfiguration }
```

##### Instances
``` purescript
Newtype PutBucketInventoryConfigurationRequest _
Generic PutBucketInventoryConfigurationRequest _
Show PutBucketInventoryConfigurationRequest
Decode PutBucketInventoryConfigurationRequest
Encode PutBucketInventoryConfigurationRequest
```

#### `newPutBucketInventoryConfigurationRequest`

``` purescript
newPutBucketInventoryConfigurationRequest :: BucketName -> InventoryId -> InventoryConfiguration -> PutBucketInventoryConfigurationRequest
```

Constructs PutBucketInventoryConfigurationRequest from required parameters

#### `newPutBucketInventoryConfigurationRequest'`

``` purescript
newPutBucketInventoryConfigurationRequest' :: BucketName -> InventoryId -> InventoryConfiguration -> ({ "Bucket" :: BucketName, "Id" :: InventoryId, "InventoryConfiguration" :: InventoryConfiguration } -> { "Bucket" :: BucketName, "Id" :: InventoryId, "InventoryConfiguration" :: InventoryConfiguration }) -> PutBucketInventoryConfigurationRequest
```

Constructs PutBucketInventoryConfigurationRequest's fields from required parameters

#### `PutBucketLifecycleConfigurationRequest`

``` purescript
newtype PutBucketLifecycleConfigurationRequest
  = PutBucketLifecycleConfigurationRequest { "Bucket" :: BucketName, "LifecycleConfiguration" :: NullOrUndefined (BucketLifecycleConfiguration) }
```

##### Instances
``` purescript
Newtype PutBucketLifecycleConfigurationRequest _
Generic PutBucketLifecycleConfigurationRequest _
Show PutBucketLifecycleConfigurationRequest
Decode PutBucketLifecycleConfigurationRequest
Encode PutBucketLifecycleConfigurationRequest
```

#### `newPutBucketLifecycleConfigurationRequest`

``` purescript
newPutBucketLifecycleConfigurationRequest :: BucketName -> PutBucketLifecycleConfigurationRequest
```

Constructs PutBucketLifecycleConfigurationRequest from required parameters

#### `newPutBucketLifecycleConfigurationRequest'`

``` purescript
newPutBucketLifecycleConfigurationRequest' :: BucketName -> ({ "Bucket" :: BucketName, "LifecycleConfiguration" :: NullOrUndefined (BucketLifecycleConfiguration) } -> { "Bucket" :: BucketName, "LifecycleConfiguration" :: NullOrUndefined (BucketLifecycleConfiguration) }) -> PutBucketLifecycleConfigurationRequest
```

Constructs PutBucketLifecycleConfigurationRequest's fields from required parameters

#### `PutBucketLifecycleRequest`

``` purescript
newtype PutBucketLifecycleRequest
  = PutBucketLifecycleRequest { "Bucket" :: BucketName, "ContentMD5" :: NullOrUndefined (ContentMD5), "LifecycleConfiguration" :: NullOrUndefined (LifecycleConfiguration) }
```

##### Instances
``` purescript
Newtype PutBucketLifecycleRequest _
Generic PutBucketLifecycleRequest _
Show PutBucketLifecycleRequest
Decode PutBucketLifecycleRequest
Encode PutBucketLifecycleRequest
```

#### `newPutBucketLifecycleRequest`

``` purescript
newPutBucketLifecycleRequest :: BucketName -> PutBucketLifecycleRequest
```

Constructs PutBucketLifecycleRequest from required parameters

#### `newPutBucketLifecycleRequest'`

``` purescript
newPutBucketLifecycleRequest' :: BucketName -> ({ "Bucket" :: BucketName, "ContentMD5" :: NullOrUndefined (ContentMD5), "LifecycleConfiguration" :: NullOrUndefined (LifecycleConfiguration) } -> { "Bucket" :: BucketName, "ContentMD5" :: NullOrUndefined (ContentMD5), "LifecycleConfiguration" :: NullOrUndefined (LifecycleConfiguration) }) -> PutBucketLifecycleRequest
```

Constructs PutBucketLifecycleRequest's fields from required parameters

#### `PutBucketLoggingRequest`

``` purescript
newtype PutBucketLoggingRequest
  = PutBucketLoggingRequest { "Bucket" :: BucketName, "BucketLoggingStatus" :: BucketLoggingStatus, "ContentMD5" :: NullOrUndefined (ContentMD5) }
```

##### Instances
``` purescript
Newtype PutBucketLoggingRequest _
Generic PutBucketLoggingRequest _
Show PutBucketLoggingRequest
Decode PutBucketLoggingRequest
Encode PutBucketLoggingRequest
```

#### `newPutBucketLoggingRequest`

``` purescript
newPutBucketLoggingRequest :: BucketName -> BucketLoggingStatus -> PutBucketLoggingRequest
```

Constructs PutBucketLoggingRequest from required parameters

#### `newPutBucketLoggingRequest'`

``` purescript
newPutBucketLoggingRequest' :: BucketName -> BucketLoggingStatus -> ({ "Bucket" :: BucketName, "BucketLoggingStatus" :: BucketLoggingStatus, "ContentMD5" :: NullOrUndefined (ContentMD5) } -> { "Bucket" :: BucketName, "BucketLoggingStatus" :: BucketLoggingStatus, "ContentMD5" :: NullOrUndefined (ContentMD5) }) -> PutBucketLoggingRequest
```

Constructs PutBucketLoggingRequest's fields from required parameters

#### `PutBucketMetricsConfigurationRequest`

``` purescript
newtype PutBucketMetricsConfigurationRequest
  = PutBucketMetricsConfigurationRequest { "Bucket" :: BucketName, "Id" :: MetricsId, "MetricsConfiguration" :: MetricsConfiguration }
```

##### Instances
``` purescript
Newtype PutBucketMetricsConfigurationRequest _
Generic PutBucketMetricsConfigurationRequest _
Show PutBucketMetricsConfigurationRequest
Decode PutBucketMetricsConfigurationRequest
Encode PutBucketMetricsConfigurationRequest
```

#### `newPutBucketMetricsConfigurationRequest`

``` purescript
newPutBucketMetricsConfigurationRequest :: BucketName -> MetricsId -> MetricsConfiguration -> PutBucketMetricsConfigurationRequest
```

Constructs PutBucketMetricsConfigurationRequest from required parameters

#### `newPutBucketMetricsConfigurationRequest'`

``` purescript
newPutBucketMetricsConfigurationRequest' :: BucketName -> MetricsId -> MetricsConfiguration -> ({ "Bucket" :: BucketName, "Id" :: MetricsId, "MetricsConfiguration" :: MetricsConfiguration } -> { "Bucket" :: BucketName, "Id" :: MetricsId, "MetricsConfiguration" :: MetricsConfiguration }) -> PutBucketMetricsConfigurationRequest
```

Constructs PutBucketMetricsConfigurationRequest's fields from required parameters

#### `PutBucketNotificationConfigurationRequest`

``` purescript
newtype PutBucketNotificationConfigurationRequest
  = PutBucketNotificationConfigurationRequest { "Bucket" :: BucketName, "NotificationConfiguration" :: NotificationConfiguration }
```

##### Instances
``` purescript
Newtype PutBucketNotificationConfigurationRequest _
Generic PutBucketNotificationConfigurationRequest _
Show PutBucketNotificationConfigurationRequest
Decode PutBucketNotificationConfigurationRequest
Encode PutBucketNotificationConfigurationRequest
```

#### `newPutBucketNotificationConfigurationRequest`

``` purescript
newPutBucketNotificationConfigurationRequest :: BucketName -> NotificationConfiguration -> PutBucketNotificationConfigurationRequest
```

Constructs PutBucketNotificationConfigurationRequest from required parameters

#### `newPutBucketNotificationConfigurationRequest'`

``` purescript
newPutBucketNotificationConfigurationRequest' :: BucketName -> NotificationConfiguration -> ({ "Bucket" :: BucketName, "NotificationConfiguration" :: NotificationConfiguration } -> { "Bucket" :: BucketName, "NotificationConfiguration" :: NotificationConfiguration }) -> PutBucketNotificationConfigurationRequest
```

Constructs PutBucketNotificationConfigurationRequest's fields from required parameters

#### `PutBucketNotificationRequest`

``` purescript
newtype PutBucketNotificationRequest
  = PutBucketNotificationRequest { "Bucket" :: BucketName, "ContentMD5" :: NullOrUndefined (ContentMD5), "NotificationConfiguration" :: NotificationConfigurationDeprecated }
```

##### Instances
``` purescript
Newtype PutBucketNotificationRequest _
Generic PutBucketNotificationRequest _
Show PutBucketNotificationRequest
Decode PutBucketNotificationRequest
Encode PutBucketNotificationRequest
```

#### `newPutBucketNotificationRequest`

``` purescript
newPutBucketNotificationRequest :: BucketName -> NotificationConfigurationDeprecated -> PutBucketNotificationRequest
```

Constructs PutBucketNotificationRequest from required parameters

#### `newPutBucketNotificationRequest'`

``` purescript
newPutBucketNotificationRequest' :: BucketName -> NotificationConfigurationDeprecated -> ({ "Bucket" :: BucketName, "ContentMD5" :: NullOrUndefined (ContentMD5), "NotificationConfiguration" :: NotificationConfigurationDeprecated } -> { "Bucket" :: BucketName, "ContentMD5" :: NullOrUndefined (ContentMD5), "NotificationConfiguration" :: NotificationConfigurationDeprecated }) -> PutBucketNotificationRequest
```

Constructs PutBucketNotificationRequest's fields from required parameters

#### `PutBucketPolicyRequest`

``` purescript
newtype PutBucketPolicyRequest
  = PutBucketPolicyRequest { "Bucket" :: BucketName, "ContentMD5" :: NullOrUndefined (ContentMD5), "ConfirmRemoveSelfBucketAccess" :: NullOrUndefined (ConfirmRemoveSelfBucketAccess), "Policy" :: Policy }
```

##### Instances
``` purescript
Newtype PutBucketPolicyRequest _
Generic PutBucketPolicyRequest _
Show PutBucketPolicyRequest
Decode PutBucketPolicyRequest
Encode PutBucketPolicyRequest
```

#### `newPutBucketPolicyRequest`

``` purescript
newPutBucketPolicyRequest :: BucketName -> Policy -> PutBucketPolicyRequest
```

Constructs PutBucketPolicyRequest from required parameters

#### `newPutBucketPolicyRequest'`

``` purescript
newPutBucketPolicyRequest' :: BucketName -> Policy -> ({ "Bucket" :: BucketName, "ContentMD5" :: NullOrUndefined (ContentMD5), "ConfirmRemoveSelfBucketAccess" :: NullOrUndefined (ConfirmRemoveSelfBucketAccess), "Policy" :: Policy } -> { "Bucket" :: BucketName, "ContentMD5" :: NullOrUndefined (ContentMD5), "ConfirmRemoveSelfBucketAccess" :: NullOrUndefined (ConfirmRemoveSelfBucketAccess), "Policy" :: Policy }) -> PutBucketPolicyRequest
```

Constructs PutBucketPolicyRequest's fields from required parameters

#### `PutBucketReplicationRequest`

``` purescript
newtype PutBucketReplicationRequest
  = PutBucketReplicationRequest { "Bucket" :: BucketName, "ContentMD5" :: NullOrUndefined (ContentMD5), "ReplicationConfiguration" :: ReplicationConfiguration }
```

##### Instances
``` purescript
Newtype PutBucketReplicationRequest _
Generic PutBucketReplicationRequest _
Show PutBucketReplicationRequest
Decode PutBucketReplicationRequest
Encode PutBucketReplicationRequest
```

#### `newPutBucketReplicationRequest`

``` purescript
newPutBucketReplicationRequest :: BucketName -> ReplicationConfiguration -> PutBucketReplicationRequest
```

Constructs PutBucketReplicationRequest from required parameters

#### `newPutBucketReplicationRequest'`

``` purescript
newPutBucketReplicationRequest' :: BucketName -> ReplicationConfiguration -> ({ "Bucket" :: BucketName, "ContentMD5" :: NullOrUndefined (ContentMD5), "ReplicationConfiguration" :: ReplicationConfiguration } -> { "Bucket" :: BucketName, "ContentMD5" :: NullOrUndefined (ContentMD5), "ReplicationConfiguration" :: ReplicationConfiguration }) -> PutBucketReplicationRequest
```

Constructs PutBucketReplicationRequest's fields from required parameters

#### `PutBucketRequestPaymentRequest`

``` purescript
newtype PutBucketRequestPaymentRequest
  = PutBucketRequestPaymentRequest { "Bucket" :: BucketName, "ContentMD5" :: NullOrUndefined (ContentMD5), "RequestPaymentConfiguration" :: RequestPaymentConfiguration }
```

##### Instances
``` purescript
Newtype PutBucketRequestPaymentRequest _
Generic PutBucketRequestPaymentRequest _
Show PutBucketRequestPaymentRequest
Decode PutBucketRequestPaymentRequest
Encode PutBucketRequestPaymentRequest
```

#### `newPutBucketRequestPaymentRequest`

``` purescript
newPutBucketRequestPaymentRequest :: BucketName -> RequestPaymentConfiguration -> PutBucketRequestPaymentRequest
```

Constructs PutBucketRequestPaymentRequest from required parameters

#### `newPutBucketRequestPaymentRequest'`

``` purescript
newPutBucketRequestPaymentRequest' :: BucketName -> RequestPaymentConfiguration -> ({ "Bucket" :: BucketName, "ContentMD5" :: NullOrUndefined (ContentMD5), "RequestPaymentConfiguration" :: RequestPaymentConfiguration } -> { "Bucket" :: BucketName, "ContentMD5" :: NullOrUndefined (ContentMD5), "RequestPaymentConfiguration" :: RequestPaymentConfiguration }) -> PutBucketRequestPaymentRequest
```

Constructs PutBucketRequestPaymentRequest's fields from required parameters

#### `PutBucketTaggingRequest`

``` purescript
newtype PutBucketTaggingRequest
  = PutBucketTaggingRequest { "Bucket" :: BucketName, "ContentMD5" :: NullOrUndefined (ContentMD5), "Tagging" :: Tagging }
```

##### Instances
``` purescript
Newtype PutBucketTaggingRequest _
Generic PutBucketTaggingRequest _
Show PutBucketTaggingRequest
Decode PutBucketTaggingRequest
Encode PutBucketTaggingRequest
```

#### `newPutBucketTaggingRequest`

``` purescript
newPutBucketTaggingRequest :: BucketName -> Tagging -> PutBucketTaggingRequest
```

Constructs PutBucketTaggingRequest from required parameters

#### `newPutBucketTaggingRequest'`

``` purescript
newPutBucketTaggingRequest' :: BucketName -> Tagging -> ({ "Bucket" :: BucketName, "ContentMD5" :: NullOrUndefined (ContentMD5), "Tagging" :: Tagging } -> { "Bucket" :: BucketName, "ContentMD5" :: NullOrUndefined (ContentMD5), "Tagging" :: Tagging }) -> PutBucketTaggingRequest
```

Constructs PutBucketTaggingRequest's fields from required parameters

#### `PutBucketVersioningRequest`

``` purescript
newtype PutBucketVersioningRequest
  = PutBucketVersioningRequest { "Bucket" :: BucketName, "ContentMD5" :: NullOrUndefined (ContentMD5), "MFA" :: NullOrUndefined (MFA), "VersioningConfiguration" :: VersioningConfiguration }
```

##### Instances
``` purescript
Newtype PutBucketVersioningRequest _
Generic PutBucketVersioningRequest _
Show PutBucketVersioningRequest
Decode PutBucketVersioningRequest
Encode PutBucketVersioningRequest
```

#### `newPutBucketVersioningRequest`

``` purescript
newPutBucketVersioningRequest :: BucketName -> VersioningConfiguration -> PutBucketVersioningRequest
```

Constructs PutBucketVersioningRequest from required parameters

#### `newPutBucketVersioningRequest'`

``` purescript
newPutBucketVersioningRequest' :: BucketName -> VersioningConfiguration -> ({ "Bucket" :: BucketName, "ContentMD5" :: NullOrUndefined (ContentMD5), "MFA" :: NullOrUndefined (MFA), "VersioningConfiguration" :: VersioningConfiguration } -> { "Bucket" :: BucketName, "ContentMD5" :: NullOrUndefined (ContentMD5), "MFA" :: NullOrUndefined (MFA), "VersioningConfiguration" :: VersioningConfiguration }) -> PutBucketVersioningRequest
```

Constructs PutBucketVersioningRequest's fields from required parameters

#### `PutBucketWebsiteRequest`

``` purescript
newtype PutBucketWebsiteRequest
  = PutBucketWebsiteRequest { "Bucket" :: BucketName, "ContentMD5" :: NullOrUndefined (ContentMD5), "WebsiteConfiguration" :: WebsiteConfiguration }
```

##### Instances
``` purescript
Newtype PutBucketWebsiteRequest _
Generic PutBucketWebsiteRequest _
Show PutBucketWebsiteRequest
Decode PutBucketWebsiteRequest
Encode PutBucketWebsiteRequest
```

#### `newPutBucketWebsiteRequest`

``` purescript
newPutBucketWebsiteRequest :: BucketName -> WebsiteConfiguration -> PutBucketWebsiteRequest
```

Constructs PutBucketWebsiteRequest from required parameters

#### `newPutBucketWebsiteRequest'`

``` purescript
newPutBucketWebsiteRequest' :: BucketName -> WebsiteConfiguration -> ({ "Bucket" :: BucketName, "ContentMD5" :: NullOrUndefined (ContentMD5), "WebsiteConfiguration" :: WebsiteConfiguration } -> { "Bucket" :: BucketName, "ContentMD5" :: NullOrUndefined (ContentMD5), "WebsiteConfiguration" :: WebsiteConfiguration }) -> PutBucketWebsiteRequest
```

Constructs PutBucketWebsiteRequest's fields from required parameters

#### `PutObjectAclOutput`

``` purescript
newtype PutObjectAclOutput
  = PutObjectAclOutput { "RequestCharged" :: NullOrUndefined (RequestCharged) }
```

##### Instances
``` purescript
Newtype PutObjectAclOutput _
Generic PutObjectAclOutput _
Show PutObjectAclOutput
Decode PutObjectAclOutput
Encode PutObjectAclOutput
```

#### `newPutObjectAclOutput`

``` purescript
newPutObjectAclOutput :: PutObjectAclOutput
```

Constructs PutObjectAclOutput from required parameters

#### `newPutObjectAclOutput'`

``` purescript
newPutObjectAclOutput' :: ({ "RequestCharged" :: NullOrUndefined (RequestCharged) } -> { "RequestCharged" :: NullOrUndefined (RequestCharged) }) -> PutObjectAclOutput
```

Constructs PutObjectAclOutput's fields from required parameters

#### `PutObjectAclRequest`

``` purescript
newtype PutObjectAclRequest
  = PutObjectAclRequest { "ACL" :: NullOrUndefined (ObjectCannedACL), "AccessControlPolicy" :: NullOrUndefined (AccessControlPolicy), "Bucket" :: BucketName, "ContentMD5" :: NullOrUndefined (ContentMD5), "GrantFullControl" :: NullOrUndefined (GrantFullControl), "GrantRead" :: NullOrUndefined (GrantRead), "GrantReadACP" :: NullOrUndefined (GrantReadACP), "GrantWrite" :: NullOrUndefined (GrantWrite), "GrantWriteACP" :: NullOrUndefined (GrantWriteACP), "Key" :: ObjectKey, "RequestPayer" :: NullOrUndefined (RequestPayer), "VersionId" :: NullOrUndefined (ObjectVersionId) }
```

##### Instances
``` purescript
Newtype PutObjectAclRequest _
Generic PutObjectAclRequest _
Show PutObjectAclRequest
Decode PutObjectAclRequest
Encode PutObjectAclRequest
```

#### `newPutObjectAclRequest`

``` purescript
newPutObjectAclRequest :: BucketName -> ObjectKey -> PutObjectAclRequest
```

Constructs PutObjectAclRequest from required parameters

#### `newPutObjectAclRequest'`

``` purescript
newPutObjectAclRequest' :: BucketName -> ObjectKey -> ({ "ACL" :: NullOrUndefined (ObjectCannedACL), "AccessControlPolicy" :: NullOrUndefined (AccessControlPolicy), "Bucket" :: BucketName, "ContentMD5" :: NullOrUndefined (ContentMD5), "GrantFullControl" :: NullOrUndefined (GrantFullControl), "GrantRead" :: NullOrUndefined (GrantRead), "GrantReadACP" :: NullOrUndefined (GrantReadACP), "GrantWrite" :: NullOrUndefined (GrantWrite), "GrantWriteACP" :: NullOrUndefined (GrantWriteACP), "Key" :: ObjectKey, "RequestPayer" :: NullOrUndefined (RequestPayer), "VersionId" :: NullOrUndefined (ObjectVersionId) } -> { "ACL" :: NullOrUndefined (ObjectCannedACL), "AccessControlPolicy" :: NullOrUndefined (AccessControlPolicy), "Bucket" :: BucketName, "ContentMD5" :: NullOrUndefined (ContentMD5), "GrantFullControl" :: NullOrUndefined (GrantFullControl), "GrantRead" :: NullOrUndefined (GrantRead), "GrantReadACP" :: NullOrUndefined (GrantReadACP), "GrantWrite" :: NullOrUndefined (GrantWrite), "GrantWriteACP" :: NullOrUndefined (GrantWriteACP), "Key" :: ObjectKey, "RequestPayer" :: NullOrUndefined (RequestPayer), "VersionId" :: NullOrUndefined (ObjectVersionId) }) -> PutObjectAclRequest
```

Constructs PutObjectAclRequest's fields from required parameters

#### `PutObjectOutput`

``` purescript
newtype PutObjectOutput
  = PutObjectOutput { "Expiration" :: NullOrUndefined (Expiration), "ETag" :: NullOrUndefined (ETag), "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption), "VersionId" :: NullOrUndefined (ObjectVersionId), "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm), "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5), "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId), "RequestCharged" :: NullOrUndefined (RequestCharged) }
```

##### Instances
``` purescript
Newtype PutObjectOutput _
Generic PutObjectOutput _
Show PutObjectOutput
Decode PutObjectOutput
Encode PutObjectOutput
```

#### `newPutObjectOutput`

``` purescript
newPutObjectOutput :: PutObjectOutput
```

Constructs PutObjectOutput from required parameters

#### `newPutObjectOutput'`

``` purescript
newPutObjectOutput' :: ({ "Expiration" :: NullOrUndefined (Expiration), "ETag" :: NullOrUndefined (ETag), "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption), "VersionId" :: NullOrUndefined (ObjectVersionId), "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm), "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5), "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId), "RequestCharged" :: NullOrUndefined (RequestCharged) } -> { "Expiration" :: NullOrUndefined (Expiration), "ETag" :: NullOrUndefined (ETag), "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption), "VersionId" :: NullOrUndefined (ObjectVersionId), "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm), "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5), "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId), "RequestCharged" :: NullOrUndefined (RequestCharged) }) -> PutObjectOutput
```

Constructs PutObjectOutput's fields from required parameters

#### `PutObjectRequest`

``` purescript
newtype PutObjectRequest
  = PutObjectRequest { "ACL" :: NullOrUndefined (ObjectCannedACL), "Body" :: NullOrUndefined (Body), "Bucket" :: BucketName, "CacheControl" :: NullOrUndefined (CacheControl), "ContentDisposition" :: NullOrUndefined (ContentDisposition), "ContentEncoding" :: NullOrUndefined (ContentEncoding), "ContentLanguage" :: NullOrUndefined (ContentLanguage), "ContentLength" :: NullOrUndefined (ContentLength), "ContentMD5" :: NullOrUndefined (ContentMD5), "ContentType" :: NullOrUndefined (ContentType), "Expires" :: NullOrUndefined (Expires), "GrantFullControl" :: NullOrUndefined (GrantFullControl), "GrantRead" :: NullOrUndefined (GrantRead), "GrantReadACP" :: NullOrUndefined (GrantReadACP), "GrantWriteACP" :: NullOrUndefined (GrantWriteACP), "Key" :: ObjectKey, "Metadata" :: NullOrUndefined (Metadata), "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption), "StorageClass" :: NullOrUndefined (StorageClass), "WebsiteRedirectLocation" :: NullOrUndefined (WebsiteRedirectLocation), "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm), "SSECustomerKey" :: NullOrUndefined (SSECustomerKey), "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5), "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId), "RequestPayer" :: NullOrUndefined (RequestPayer), "Tagging" :: NullOrUndefined (TaggingHeader) }
```

##### Instances
``` purescript
Newtype PutObjectRequest _
Generic PutObjectRequest _
Show PutObjectRequest
Decode PutObjectRequest
Encode PutObjectRequest
```

#### `newPutObjectRequest`

``` purescript
newPutObjectRequest :: BucketName -> ObjectKey -> PutObjectRequest
```

Constructs PutObjectRequest from required parameters

#### `newPutObjectRequest'`

``` purescript
newPutObjectRequest' :: BucketName -> ObjectKey -> ({ "ACL" :: NullOrUndefined (ObjectCannedACL), "Body" :: NullOrUndefined (Body), "Bucket" :: BucketName, "CacheControl" :: NullOrUndefined (CacheControl), "ContentDisposition" :: NullOrUndefined (ContentDisposition), "ContentEncoding" :: NullOrUndefined (ContentEncoding), "ContentLanguage" :: NullOrUndefined (ContentLanguage), "ContentLength" :: NullOrUndefined (ContentLength), "ContentMD5" :: NullOrUndefined (ContentMD5), "ContentType" :: NullOrUndefined (ContentType), "Expires" :: NullOrUndefined (Expires), "GrantFullControl" :: NullOrUndefined (GrantFullControl), "GrantRead" :: NullOrUndefined (GrantRead), "GrantReadACP" :: NullOrUndefined (GrantReadACP), "GrantWriteACP" :: NullOrUndefined (GrantWriteACP), "Key" :: ObjectKey, "Metadata" :: NullOrUndefined (Metadata), "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption), "StorageClass" :: NullOrUndefined (StorageClass), "WebsiteRedirectLocation" :: NullOrUndefined (WebsiteRedirectLocation), "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm), "SSECustomerKey" :: NullOrUndefined (SSECustomerKey), "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5), "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId), "RequestPayer" :: NullOrUndefined (RequestPayer), "Tagging" :: NullOrUndefined (TaggingHeader) } -> { "ACL" :: NullOrUndefined (ObjectCannedACL), "Body" :: NullOrUndefined (Body), "Bucket" :: BucketName, "CacheControl" :: NullOrUndefined (CacheControl), "ContentDisposition" :: NullOrUndefined (ContentDisposition), "ContentEncoding" :: NullOrUndefined (ContentEncoding), "ContentLanguage" :: NullOrUndefined (ContentLanguage), "ContentLength" :: NullOrUndefined (ContentLength), "ContentMD5" :: NullOrUndefined (ContentMD5), "ContentType" :: NullOrUndefined (ContentType), "Expires" :: NullOrUndefined (Expires), "GrantFullControl" :: NullOrUndefined (GrantFullControl), "GrantRead" :: NullOrUndefined (GrantRead), "GrantReadACP" :: NullOrUndefined (GrantReadACP), "GrantWriteACP" :: NullOrUndefined (GrantWriteACP), "Key" :: ObjectKey, "Metadata" :: NullOrUndefined (Metadata), "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption), "StorageClass" :: NullOrUndefined (StorageClass), "WebsiteRedirectLocation" :: NullOrUndefined (WebsiteRedirectLocation), "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm), "SSECustomerKey" :: NullOrUndefined (SSECustomerKey), "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5), "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId), "RequestPayer" :: NullOrUndefined (RequestPayer), "Tagging" :: NullOrUndefined (TaggingHeader) }) -> PutObjectRequest
```

Constructs PutObjectRequest's fields from required parameters

#### `PutObjectTaggingOutput`

``` purescript
newtype PutObjectTaggingOutput
  = PutObjectTaggingOutput { "VersionId" :: NullOrUndefined (ObjectVersionId) }
```

##### Instances
``` purescript
Newtype PutObjectTaggingOutput _
Generic PutObjectTaggingOutput _
Show PutObjectTaggingOutput
Decode PutObjectTaggingOutput
Encode PutObjectTaggingOutput
```

#### `newPutObjectTaggingOutput`

``` purescript
newPutObjectTaggingOutput :: PutObjectTaggingOutput
```

Constructs PutObjectTaggingOutput from required parameters

#### `newPutObjectTaggingOutput'`

``` purescript
newPutObjectTaggingOutput' :: ({ "VersionId" :: NullOrUndefined (ObjectVersionId) } -> { "VersionId" :: NullOrUndefined (ObjectVersionId) }) -> PutObjectTaggingOutput
```

Constructs PutObjectTaggingOutput's fields from required parameters

#### `PutObjectTaggingRequest`

``` purescript
newtype PutObjectTaggingRequest
  = PutObjectTaggingRequest { "Bucket" :: BucketName, "Key" :: ObjectKey, "VersionId" :: NullOrUndefined (ObjectVersionId), "ContentMD5" :: NullOrUndefined (ContentMD5), "Tagging" :: Tagging }
```

##### Instances
``` purescript
Newtype PutObjectTaggingRequest _
Generic PutObjectTaggingRequest _
Show PutObjectTaggingRequest
Decode PutObjectTaggingRequest
Encode PutObjectTaggingRequest
```

#### `newPutObjectTaggingRequest`

``` purescript
newPutObjectTaggingRequest :: BucketName -> ObjectKey -> Tagging -> PutObjectTaggingRequest
```

Constructs PutObjectTaggingRequest from required parameters

#### `newPutObjectTaggingRequest'`

``` purescript
newPutObjectTaggingRequest' :: BucketName -> ObjectKey -> Tagging -> ({ "Bucket" :: BucketName, "Key" :: ObjectKey, "VersionId" :: NullOrUndefined (ObjectVersionId), "ContentMD5" :: NullOrUndefined (ContentMD5), "Tagging" :: Tagging } -> { "Bucket" :: BucketName, "Key" :: ObjectKey, "VersionId" :: NullOrUndefined (ObjectVersionId), "ContentMD5" :: NullOrUndefined (ContentMD5), "Tagging" :: Tagging }) -> PutObjectTaggingRequest
```

Constructs PutObjectTaggingRequest's fields from required parameters

#### `QueueArn`

``` purescript
newtype QueueArn
  = QueueArn String
```

##### Instances
``` purescript
Newtype QueueArn _
Generic QueueArn _
Show QueueArn
Decode QueueArn
Encode QueueArn
```

#### `QueueConfiguration`

``` purescript
newtype QueueConfiguration
  = QueueConfiguration { "Id" :: NullOrUndefined (NotificationId), "QueueArn" :: QueueArn, "Events" :: EventList, "Filter" :: NullOrUndefined (NotificationConfigurationFilter) }
```

Container for specifying an configuration when you want Amazon S3 to publish events to an Amazon Simple Queue Service (Amazon SQS) queue.

##### Instances
``` purescript
Newtype QueueConfiguration _
Generic QueueConfiguration _
Show QueueConfiguration
Decode QueueConfiguration
Encode QueueConfiguration
```

#### `newQueueConfiguration`

``` purescript
newQueueConfiguration :: EventList -> QueueArn -> QueueConfiguration
```

Constructs QueueConfiguration from required parameters

#### `newQueueConfiguration'`

``` purescript
newQueueConfiguration' :: EventList -> QueueArn -> ({ "Id" :: NullOrUndefined (NotificationId), "QueueArn" :: QueueArn, "Events" :: EventList, "Filter" :: NullOrUndefined (NotificationConfigurationFilter) } -> { "Id" :: NullOrUndefined (NotificationId), "QueueArn" :: QueueArn, "Events" :: EventList, "Filter" :: NullOrUndefined (NotificationConfigurationFilter) }) -> QueueConfiguration
```

Constructs QueueConfiguration's fields from required parameters

#### `QueueConfigurationDeprecated`

``` purescript
newtype QueueConfigurationDeprecated
  = QueueConfigurationDeprecated { "Id" :: NullOrUndefined (NotificationId), "Event" :: NullOrUndefined (Event), "Events" :: NullOrUndefined (EventList), "Queue" :: NullOrUndefined (QueueArn) }
```

##### Instances
``` purescript
Newtype QueueConfigurationDeprecated _
Generic QueueConfigurationDeprecated _
Show QueueConfigurationDeprecated
Decode QueueConfigurationDeprecated
Encode QueueConfigurationDeprecated
```

#### `newQueueConfigurationDeprecated`

``` purescript
newQueueConfigurationDeprecated :: QueueConfigurationDeprecated
```

Constructs QueueConfigurationDeprecated from required parameters

#### `newQueueConfigurationDeprecated'`

``` purescript
newQueueConfigurationDeprecated' :: ({ "Id" :: NullOrUndefined (NotificationId), "Event" :: NullOrUndefined (Event), "Events" :: NullOrUndefined (EventList), "Queue" :: NullOrUndefined (QueueArn) } -> { "Id" :: NullOrUndefined (NotificationId), "Event" :: NullOrUndefined (Event), "Events" :: NullOrUndefined (EventList), "Queue" :: NullOrUndefined (QueueArn) }) -> QueueConfigurationDeprecated
```

Constructs QueueConfigurationDeprecated's fields from required parameters

#### `QueueConfigurationList`

``` purescript
newtype QueueConfigurationList
  = QueueConfigurationList (Array QueueConfiguration)
```

##### Instances
``` purescript
Newtype QueueConfigurationList _
Generic QueueConfigurationList _
Show QueueConfigurationList
Decode QueueConfigurationList
Encode QueueConfigurationList
```

#### `Quiet`

``` purescript
newtype Quiet
  = Quiet Boolean
```

##### Instances
``` purescript
Newtype Quiet _
Generic Quiet _
Show Quiet
Decode Quiet
Encode Quiet
```

#### `QuoteCharacter`

``` purescript
newtype QuoteCharacter
  = QuoteCharacter String
```

##### Instances
``` purescript
Newtype QuoteCharacter _
Generic QuoteCharacter _
Show QuoteCharacter
Decode QuoteCharacter
Encode QuoteCharacter
```

#### `QuoteEscapeCharacter`

``` purescript
newtype QuoteEscapeCharacter
  = QuoteEscapeCharacter String
```

##### Instances
``` purescript
Newtype QuoteEscapeCharacter _
Generic QuoteEscapeCharacter _
Show QuoteEscapeCharacter
Decode QuoteEscapeCharacter
Encode QuoteEscapeCharacter
```

#### `QuoteFields`

``` purescript
newtype QuoteFields
  = QuoteFields String
```

##### Instances
``` purescript
Newtype QuoteFields _
Generic QuoteFields _
Show QuoteFields
Decode QuoteFields
Encode QuoteFields
```

#### `Range`

``` purescript
newtype Range
  = Range String
```

##### Instances
``` purescript
Newtype Range _
Generic Range _
Show Range
Decode Range
Encode Range
```

#### `RecordDelimiter`

``` purescript
newtype RecordDelimiter
  = RecordDelimiter String
```

##### Instances
``` purescript
Newtype RecordDelimiter _
Generic RecordDelimiter _
Show RecordDelimiter
Decode RecordDelimiter
Encode RecordDelimiter
```

#### `Redirect`

``` purescript
newtype Redirect
  = Redirect { "HostName" :: NullOrUndefined (HostName), "HttpRedirectCode" :: NullOrUndefined (HttpRedirectCode), "Protocol" :: NullOrUndefined (Protocol), "ReplaceKeyPrefixWith" :: NullOrUndefined (ReplaceKeyPrefixWith), "ReplaceKeyWith" :: NullOrUndefined (ReplaceKeyWith) }
```

##### Instances
``` purescript
Newtype Redirect _
Generic Redirect _
Show Redirect
Decode Redirect
Encode Redirect
```

#### `newRedirect`

``` purescript
newRedirect :: Redirect
```

Constructs Redirect from required parameters

#### `newRedirect'`

``` purescript
newRedirect' :: ({ "HostName" :: NullOrUndefined (HostName), "HttpRedirectCode" :: NullOrUndefined (HttpRedirectCode), "Protocol" :: NullOrUndefined (Protocol), "ReplaceKeyPrefixWith" :: NullOrUndefined (ReplaceKeyPrefixWith), "ReplaceKeyWith" :: NullOrUndefined (ReplaceKeyWith) } -> { "HostName" :: NullOrUndefined (HostName), "HttpRedirectCode" :: NullOrUndefined (HttpRedirectCode), "Protocol" :: NullOrUndefined (Protocol), "ReplaceKeyPrefixWith" :: NullOrUndefined (ReplaceKeyPrefixWith), "ReplaceKeyWith" :: NullOrUndefined (ReplaceKeyWith) }) -> Redirect
```

Constructs Redirect's fields from required parameters

#### `RedirectAllRequestsTo`

``` purescript
newtype RedirectAllRequestsTo
  = RedirectAllRequestsTo { "HostName" :: HostName, "Protocol" :: NullOrUndefined (Protocol) }
```

##### Instances
``` purescript
Newtype RedirectAllRequestsTo _
Generic RedirectAllRequestsTo _
Show RedirectAllRequestsTo
Decode RedirectAllRequestsTo
Encode RedirectAllRequestsTo
```

#### `newRedirectAllRequestsTo`

``` purescript
newRedirectAllRequestsTo :: HostName -> RedirectAllRequestsTo
```

Constructs RedirectAllRequestsTo from required parameters

#### `newRedirectAllRequestsTo'`

``` purescript
newRedirectAllRequestsTo' :: HostName -> ({ "HostName" :: HostName, "Protocol" :: NullOrUndefined (Protocol) } -> { "HostName" :: HostName, "Protocol" :: NullOrUndefined (Protocol) }) -> RedirectAllRequestsTo
```

Constructs RedirectAllRequestsTo's fields from required parameters

#### `ReplaceKeyPrefixWith`

``` purescript
newtype ReplaceKeyPrefixWith
  = ReplaceKeyPrefixWith String
```

##### Instances
``` purescript
Newtype ReplaceKeyPrefixWith _
Generic ReplaceKeyPrefixWith _
Show ReplaceKeyPrefixWith
Decode ReplaceKeyPrefixWith
Encode ReplaceKeyPrefixWith
```

#### `ReplaceKeyWith`

``` purescript
newtype ReplaceKeyWith
  = ReplaceKeyWith String
```

##### Instances
``` purescript
Newtype ReplaceKeyWith _
Generic ReplaceKeyWith _
Show ReplaceKeyWith
Decode ReplaceKeyWith
Encode ReplaceKeyWith
```

#### `ReplicaKmsKeyID`

``` purescript
newtype ReplicaKmsKeyID
  = ReplicaKmsKeyID String
```

##### Instances
``` purescript
Newtype ReplicaKmsKeyID _
Generic ReplicaKmsKeyID _
Show ReplicaKmsKeyID
Decode ReplicaKmsKeyID
Encode ReplicaKmsKeyID
```

#### `ReplicationConfiguration`

``` purescript
newtype ReplicationConfiguration
  = ReplicationConfiguration { "Role" :: Role, "Rules" :: ReplicationRules }
```

Container for replication rules. You can add as many as 1,000 rules. Total replication configuration size can be up to 2 MB.

##### Instances
``` purescript
Newtype ReplicationConfiguration _
Generic ReplicationConfiguration _
Show ReplicationConfiguration
Decode ReplicationConfiguration
Encode ReplicationConfiguration
```

#### `newReplicationConfiguration`

``` purescript
newReplicationConfiguration :: Role -> ReplicationRules -> ReplicationConfiguration
```

Constructs ReplicationConfiguration from required parameters

#### `newReplicationConfiguration'`

``` purescript
newReplicationConfiguration' :: Role -> ReplicationRules -> ({ "Role" :: Role, "Rules" :: ReplicationRules } -> { "Role" :: Role, "Rules" :: ReplicationRules }) -> ReplicationConfiguration
```

Constructs ReplicationConfiguration's fields from required parameters

#### `ReplicationRule`

``` purescript
newtype ReplicationRule
  = ReplicationRule { "ID" :: NullOrUndefined (ID), "Prefix" :: Prefix, "Status" :: ReplicationRuleStatus, "SourceSelectionCriteria" :: NullOrUndefined (SourceSelectionCriteria), "Destination" :: Destination }
```

Container for information about a particular replication rule.

##### Instances
``` purescript
Newtype ReplicationRule _
Generic ReplicationRule _
Show ReplicationRule
Decode ReplicationRule
Encode ReplicationRule
```

#### `newReplicationRule`

``` purescript
newReplicationRule :: Destination -> Prefix -> ReplicationRuleStatus -> ReplicationRule
```

Constructs ReplicationRule from required parameters

#### `newReplicationRule'`

``` purescript
newReplicationRule' :: Destination -> Prefix -> ReplicationRuleStatus -> ({ "ID" :: NullOrUndefined (ID), "Prefix" :: Prefix, "Status" :: ReplicationRuleStatus, "SourceSelectionCriteria" :: NullOrUndefined (SourceSelectionCriteria), "Destination" :: Destination } -> { "ID" :: NullOrUndefined (ID), "Prefix" :: Prefix, "Status" :: ReplicationRuleStatus, "SourceSelectionCriteria" :: NullOrUndefined (SourceSelectionCriteria), "Destination" :: Destination }) -> ReplicationRule
```

Constructs ReplicationRule's fields from required parameters

#### `ReplicationRuleStatus`

``` purescript
newtype ReplicationRuleStatus
  = ReplicationRuleStatus String
```

##### Instances
``` purescript
Newtype ReplicationRuleStatus _
Generic ReplicationRuleStatus _
Show ReplicationRuleStatus
Decode ReplicationRuleStatus
Encode ReplicationRuleStatus
```

#### `ReplicationRules`

``` purescript
newtype ReplicationRules
  = ReplicationRules (Array ReplicationRule)
```

##### Instances
``` purescript
Newtype ReplicationRules _
Generic ReplicationRules _
Show ReplicationRules
Decode ReplicationRules
Encode ReplicationRules
```

#### `ReplicationStatus`

``` purescript
newtype ReplicationStatus
  = ReplicationStatus String
```

##### Instances
``` purescript
Newtype ReplicationStatus _
Generic ReplicationStatus _
Show ReplicationStatus
Decode ReplicationStatus
Encode ReplicationStatus
```

#### `RequestCharged`

``` purescript
newtype RequestCharged
  = RequestCharged String
```

If present, indicates that the requester was successfully charged for the request.

##### Instances
``` purescript
Newtype RequestCharged _
Generic RequestCharged _
Show RequestCharged
Decode RequestCharged
Encode RequestCharged
```

#### `RequestPayer`

``` purescript
newtype RequestPayer
  = RequestPayer String
```

Confirms that the requester knows that she or he will be charged for the request. Bucket owners need not specify this parameter in their requests. Documentation on downloading objects from requester pays buckets can be found at http://docs.aws.amazon.com/AmazonS3/latest/dev/ObjectsinRequesterPaysBuckets.html

##### Instances
``` purescript
Newtype RequestPayer _
Generic RequestPayer _
Show RequestPayer
Decode RequestPayer
Encode RequestPayer
```

#### `RequestPaymentConfiguration`

``` purescript
newtype RequestPaymentConfiguration
  = RequestPaymentConfiguration { "Payer" :: Payer }
```

##### Instances
``` purescript
Newtype RequestPaymentConfiguration _
Generic RequestPaymentConfiguration _
Show RequestPaymentConfiguration
Decode RequestPaymentConfiguration
Encode RequestPaymentConfiguration
```

#### `newRequestPaymentConfiguration`

``` purescript
newRequestPaymentConfiguration :: Payer -> RequestPaymentConfiguration
```

Constructs RequestPaymentConfiguration from required parameters

#### `newRequestPaymentConfiguration'`

``` purescript
newRequestPaymentConfiguration' :: Payer -> ({ "Payer" :: Payer } -> { "Payer" :: Payer }) -> RequestPaymentConfiguration
```

Constructs RequestPaymentConfiguration's fields from required parameters

#### `ResponseCacheControl`

``` purescript
newtype ResponseCacheControl
  = ResponseCacheControl String
```

##### Instances
``` purescript
Newtype ResponseCacheControl _
Generic ResponseCacheControl _
Show ResponseCacheControl
Decode ResponseCacheControl
Encode ResponseCacheControl
```

#### `ResponseContentDisposition`

``` purescript
newtype ResponseContentDisposition
  = ResponseContentDisposition String
```

##### Instances
``` purescript
Newtype ResponseContentDisposition _
Generic ResponseContentDisposition _
Show ResponseContentDisposition
Decode ResponseContentDisposition
Encode ResponseContentDisposition
```

#### `ResponseContentEncoding`

``` purescript
newtype ResponseContentEncoding
  = ResponseContentEncoding String
```

##### Instances
``` purescript
Newtype ResponseContentEncoding _
Generic ResponseContentEncoding _
Show ResponseContentEncoding
Decode ResponseContentEncoding
Encode ResponseContentEncoding
```

#### `ResponseContentLanguage`

``` purescript
newtype ResponseContentLanguage
  = ResponseContentLanguage String
```

##### Instances
``` purescript
Newtype ResponseContentLanguage _
Generic ResponseContentLanguage _
Show ResponseContentLanguage
Decode ResponseContentLanguage
Encode ResponseContentLanguage
```

#### `ResponseContentType`

``` purescript
newtype ResponseContentType
  = ResponseContentType String
```

##### Instances
``` purescript
Newtype ResponseContentType _
Generic ResponseContentType _
Show ResponseContentType
Decode ResponseContentType
Encode ResponseContentType
```

#### `ResponseExpires`

``` purescript
newtype ResponseExpires
  = ResponseExpires Timestamp
```

##### Instances
``` purescript
Newtype ResponseExpires _
Generic ResponseExpires _
Show ResponseExpires
Decode ResponseExpires
Encode ResponseExpires
```

#### `Restore`

``` purescript
newtype Restore
  = Restore String
```

##### Instances
``` purescript
Newtype Restore _
Generic Restore _
Show Restore
Decode Restore
Encode Restore
```

#### `RestoreObjectOutput`

``` purescript
newtype RestoreObjectOutput
  = RestoreObjectOutput { "RequestCharged" :: NullOrUndefined (RequestCharged), "RestoreOutputPath" :: NullOrUndefined (RestoreOutputPath) }
```

##### Instances
``` purescript
Newtype RestoreObjectOutput _
Generic RestoreObjectOutput _
Show RestoreObjectOutput
Decode RestoreObjectOutput
Encode RestoreObjectOutput
```

#### `newRestoreObjectOutput`

``` purescript
newRestoreObjectOutput :: RestoreObjectOutput
```

Constructs RestoreObjectOutput from required parameters

#### `newRestoreObjectOutput'`

``` purescript
newRestoreObjectOutput' :: ({ "RequestCharged" :: NullOrUndefined (RequestCharged), "RestoreOutputPath" :: NullOrUndefined (RestoreOutputPath) } -> { "RequestCharged" :: NullOrUndefined (RequestCharged), "RestoreOutputPath" :: NullOrUndefined (RestoreOutputPath) }) -> RestoreObjectOutput
```

Constructs RestoreObjectOutput's fields from required parameters

#### `RestoreObjectRequest`

``` purescript
newtype RestoreObjectRequest
  = RestoreObjectRequest { "Bucket" :: BucketName, "Key" :: ObjectKey, "VersionId" :: NullOrUndefined (ObjectVersionId), "RestoreRequest" :: NullOrUndefined (RestoreRequest), "RequestPayer" :: NullOrUndefined (RequestPayer) }
```

##### Instances
``` purescript
Newtype RestoreObjectRequest _
Generic RestoreObjectRequest _
Show RestoreObjectRequest
Decode RestoreObjectRequest
Encode RestoreObjectRequest
```

#### `newRestoreObjectRequest`

``` purescript
newRestoreObjectRequest :: BucketName -> ObjectKey -> RestoreObjectRequest
```

Constructs RestoreObjectRequest from required parameters

#### `newRestoreObjectRequest'`

``` purescript
newRestoreObjectRequest' :: BucketName -> ObjectKey -> ({ "Bucket" :: BucketName, "Key" :: ObjectKey, "VersionId" :: NullOrUndefined (ObjectVersionId), "RestoreRequest" :: NullOrUndefined (RestoreRequest), "RequestPayer" :: NullOrUndefined (RequestPayer) } -> { "Bucket" :: BucketName, "Key" :: ObjectKey, "VersionId" :: NullOrUndefined (ObjectVersionId), "RestoreRequest" :: NullOrUndefined (RestoreRequest), "RequestPayer" :: NullOrUndefined (RequestPayer) }) -> RestoreObjectRequest
```

Constructs RestoreObjectRequest's fields from required parameters

#### `RestoreOutputPath`

``` purescript
newtype RestoreOutputPath
  = RestoreOutputPath String
```

##### Instances
``` purescript
Newtype RestoreOutputPath _
Generic RestoreOutputPath _
Show RestoreOutputPath
Decode RestoreOutputPath
Encode RestoreOutputPath
```

#### `RestoreRequest`

``` purescript
newtype RestoreRequest
  = RestoreRequest { "Days" :: NullOrUndefined (Days), "GlacierJobParameters" :: NullOrUndefined (GlacierJobParameters), "Type" :: NullOrUndefined (RestoreRequestType), "Tier" :: NullOrUndefined (Tier), "Description" :: NullOrUndefined (Description), "SelectParameters" :: NullOrUndefined (SelectParameters), "OutputLocation" :: NullOrUndefined (OutputLocation) }
```

Container for restore job parameters.

##### Instances
``` purescript
Newtype RestoreRequest _
Generic RestoreRequest _
Show RestoreRequest
Decode RestoreRequest
Encode RestoreRequest
```

#### `newRestoreRequest`

``` purescript
newRestoreRequest :: RestoreRequest
```

Constructs RestoreRequest from required parameters

#### `newRestoreRequest'`

``` purescript
newRestoreRequest' :: ({ "Days" :: NullOrUndefined (Days), "GlacierJobParameters" :: NullOrUndefined (GlacierJobParameters), "Type" :: NullOrUndefined (RestoreRequestType), "Tier" :: NullOrUndefined (Tier), "Description" :: NullOrUndefined (Description), "SelectParameters" :: NullOrUndefined (SelectParameters), "OutputLocation" :: NullOrUndefined (OutputLocation) } -> { "Days" :: NullOrUndefined (Days), "GlacierJobParameters" :: NullOrUndefined (GlacierJobParameters), "Type" :: NullOrUndefined (RestoreRequestType), "Tier" :: NullOrUndefined (Tier), "Description" :: NullOrUndefined (Description), "SelectParameters" :: NullOrUndefined (SelectParameters), "OutputLocation" :: NullOrUndefined (OutputLocation) }) -> RestoreRequest
```

Constructs RestoreRequest's fields from required parameters

#### `RestoreRequestType`

``` purescript
newtype RestoreRequestType
  = RestoreRequestType String
```

##### Instances
``` purescript
Newtype RestoreRequestType _
Generic RestoreRequestType _
Show RestoreRequestType
Decode RestoreRequestType
Encode RestoreRequestType
```

#### `Role`

``` purescript
newtype Role
  = Role String
```

##### Instances
``` purescript
Newtype Role _
Generic Role _
Show Role
Decode Role
Encode Role
```

#### `RoutingRule`

``` purescript
newtype RoutingRule
  = RoutingRule { "Condition" :: NullOrUndefined (Condition), "Redirect" :: Redirect }
```

##### Instances
``` purescript
Newtype RoutingRule _
Generic RoutingRule _
Show RoutingRule
Decode RoutingRule
Encode RoutingRule
```

#### `newRoutingRule`

``` purescript
newRoutingRule :: Redirect -> RoutingRule
```

Constructs RoutingRule from required parameters

#### `newRoutingRule'`

``` purescript
newRoutingRule' :: Redirect -> ({ "Condition" :: NullOrUndefined (Condition), "Redirect" :: Redirect } -> { "Condition" :: NullOrUndefined (Condition), "Redirect" :: Redirect }) -> RoutingRule
```

Constructs RoutingRule's fields from required parameters

#### `RoutingRules`

``` purescript
newtype RoutingRules
  = RoutingRules (Array RoutingRule)
```

##### Instances
``` purescript
Newtype RoutingRules _
Generic RoutingRules _
Show RoutingRules
Decode RoutingRules
Encode RoutingRules
```

#### `Rule`

``` purescript
newtype Rule
  = Rule { "Expiration" :: NullOrUndefined (LifecycleExpiration), "ID" :: NullOrUndefined (ID), "Prefix" :: Prefix, "Status" :: ExpirationStatus, "Transition" :: NullOrUndefined (Transition), "NoncurrentVersionTransition" :: NullOrUndefined (NoncurrentVersionTransition), "NoncurrentVersionExpiration" :: NullOrUndefined (NoncurrentVersionExpiration), "AbortIncompleteMultipartUpload" :: NullOrUndefined (AbortIncompleteMultipartUpload) }
```

##### Instances
``` purescript
Newtype Rule _
Generic Rule _
Show Rule
Decode Rule
Encode Rule
```

#### `newRule`

``` purescript
newRule :: Prefix -> ExpirationStatus -> Rule
```

Constructs Rule from required parameters

#### `newRule'`

``` purescript
newRule' :: Prefix -> ExpirationStatus -> ({ "Expiration" :: NullOrUndefined (LifecycleExpiration), "ID" :: NullOrUndefined (ID), "Prefix" :: Prefix, "Status" :: ExpirationStatus, "Transition" :: NullOrUndefined (Transition), "NoncurrentVersionTransition" :: NullOrUndefined (NoncurrentVersionTransition), "NoncurrentVersionExpiration" :: NullOrUndefined (NoncurrentVersionExpiration), "AbortIncompleteMultipartUpload" :: NullOrUndefined (AbortIncompleteMultipartUpload) } -> { "Expiration" :: NullOrUndefined (LifecycleExpiration), "ID" :: NullOrUndefined (ID), "Prefix" :: Prefix, "Status" :: ExpirationStatus, "Transition" :: NullOrUndefined (Transition), "NoncurrentVersionTransition" :: NullOrUndefined (NoncurrentVersionTransition), "NoncurrentVersionExpiration" :: NullOrUndefined (NoncurrentVersionExpiration), "AbortIncompleteMultipartUpload" :: NullOrUndefined (AbortIncompleteMultipartUpload) }) -> Rule
```

Constructs Rule's fields from required parameters

#### `Rules`

``` purescript
newtype Rules
  = Rules (Array Rule)
```

##### Instances
``` purescript
Newtype Rules _
Generic Rules _
Show Rules
Decode Rules
Encode Rules
```

#### `S3KeyFilter`

``` purescript
newtype S3KeyFilter
  = S3KeyFilter { "FilterRules" :: NullOrUndefined (FilterRuleList) }
```

Container for object key name prefix and suffix filtering rules.

##### Instances
``` purescript
Newtype S3KeyFilter _
Generic S3KeyFilter _
Show S3KeyFilter
Decode S3KeyFilter
Encode S3KeyFilter
```

#### `newS3KeyFilter`

``` purescript
newS3KeyFilter :: S3KeyFilter
```

Constructs S3KeyFilter from required parameters

#### `newS3KeyFilter'`

``` purescript
newS3KeyFilter' :: ({ "FilterRules" :: NullOrUndefined (FilterRuleList) } -> { "FilterRules" :: NullOrUndefined (FilterRuleList) }) -> S3KeyFilter
```

Constructs S3KeyFilter's fields from required parameters

#### `S3Location`

``` purescript
newtype S3Location
  = S3Location { "BucketName" :: BucketName, "Prefix" :: LocationPrefix, "Encryption" :: NullOrUndefined (Encryption), "CannedACL" :: NullOrUndefined (ObjectCannedACL), "AccessControlList" :: NullOrUndefined (Grants), "Tagging" :: NullOrUndefined (Tagging), "UserMetadata" :: NullOrUndefined (UserMetadata), "StorageClass" :: NullOrUndefined (StorageClass) }
```

Describes an S3 location that will receive the results of the restore request.

##### Instances
``` purescript
Newtype S3Location _
Generic S3Location _
Show S3Location
Decode S3Location
Encode S3Location
```

#### `newS3Location`

``` purescript
newS3Location :: BucketName -> LocationPrefix -> S3Location
```

Constructs S3Location from required parameters

#### `newS3Location'`

``` purescript
newS3Location' :: BucketName -> LocationPrefix -> ({ "BucketName" :: BucketName, "Prefix" :: LocationPrefix, "Encryption" :: NullOrUndefined (Encryption), "CannedACL" :: NullOrUndefined (ObjectCannedACL), "AccessControlList" :: NullOrUndefined (Grants), "Tagging" :: NullOrUndefined (Tagging), "UserMetadata" :: NullOrUndefined (UserMetadata), "StorageClass" :: NullOrUndefined (StorageClass) } -> { "BucketName" :: BucketName, "Prefix" :: LocationPrefix, "Encryption" :: NullOrUndefined (Encryption), "CannedACL" :: NullOrUndefined (ObjectCannedACL), "AccessControlList" :: NullOrUndefined (Grants), "Tagging" :: NullOrUndefined (Tagging), "UserMetadata" :: NullOrUndefined (UserMetadata), "StorageClass" :: NullOrUndefined (StorageClass) }) -> S3Location
```

Constructs S3Location's fields from required parameters

#### `SSECustomerAlgorithm`

``` purescript
newtype SSECustomerAlgorithm
  = SSECustomerAlgorithm String
```

##### Instances
``` purescript
Newtype SSECustomerAlgorithm _
Generic SSECustomerAlgorithm _
Show SSECustomerAlgorithm
Decode SSECustomerAlgorithm
Encode SSECustomerAlgorithm
```

#### `SSECustomerKey`

``` purescript
newtype SSECustomerKey
  = SSECustomerKey String
```

##### Instances
``` purescript
Newtype SSECustomerKey _
Generic SSECustomerKey _
Show SSECustomerKey
Decode SSECustomerKey
Encode SSECustomerKey
```

#### `SSECustomerKeyMD5`

``` purescript
newtype SSECustomerKeyMD5
  = SSECustomerKeyMD5 String
```

##### Instances
``` purescript
Newtype SSECustomerKeyMD5 _
Generic SSECustomerKeyMD5 _
Show SSECustomerKeyMD5
Decode SSECustomerKeyMD5
Encode SSECustomerKeyMD5
```

#### `SSEKMS`

``` purescript
newtype SSEKMS
  = SSEKMS { "KeyId" :: SSEKMSKeyId }
```

Specifies the use of SSE-KMS to encrypt delievered Inventory reports.

##### Instances
``` purescript
Newtype SSEKMS _
Generic SSEKMS _
Show SSEKMS
Decode SSEKMS
Encode SSEKMS
```

#### `newSSEKMS`

``` purescript
newSSEKMS :: SSEKMSKeyId -> SSEKMS
```

Constructs SSEKMS from required parameters

#### `newSSEKMS'`

``` purescript
newSSEKMS' :: SSEKMSKeyId -> ({ "KeyId" :: SSEKMSKeyId } -> { "KeyId" :: SSEKMSKeyId }) -> SSEKMS
```

Constructs SSEKMS's fields from required parameters

#### `SSEKMSKeyId`

``` purescript
newtype SSEKMSKeyId
  = SSEKMSKeyId String
```

##### Instances
``` purescript
Newtype SSEKMSKeyId _
Generic SSEKMSKeyId _
Show SSEKMSKeyId
Decode SSEKMSKeyId
Encode SSEKMSKeyId
```

#### `SSES3`

``` purescript
newtype SSES3
  = SSES3 NoArguments
```

Specifies the use of SSE-S3 to encrypt delievered Inventory reports.

##### Instances
``` purescript
Newtype SSES3 _
Generic SSES3 _
Show SSES3
Decode SSES3
Encode SSES3
```

#### `SelectParameters`

``` purescript
newtype SelectParameters
  = SelectParameters { "InputSerialization" :: InputSerialization, "ExpressionType" :: ExpressionType, "Expression" :: Expression, "OutputSerialization" :: OutputSerialization }
```

Describes the parameters for Select job types.

##### Instances
``` purescript
Newtype SelectParameters _
Generic SelectParameters _
Show SelectParameters
Decode SelectParameters
Encode SelectParameters
```

#### `newSelectParameters`

``` purescript
newSelectParameters :: Expression -> ExpressionType -> InputSerialization -> OutputSerialization -> SelectParameters
```

Constructs SelectParameters from required parameters

#### `newSelectParameters'`

``` purescript
newSelectParameters' :: Expression -> ExpressionType -> InputSerialization -> OutputSerialization -> ({ "InputSerialization" :: InputSerialization, "ExpressionType" :: ExpressionType, "Expression" :: Expression, "OutputSerialization" :: OutputSerialization } -> { "InputSerialization" :: InputSerialization, "ExpressionType" :: ExpressionType, "Expression" :: Expression, "OutputSerialization" :: OutputSerialization }) -> SelectParameters
```

Constructs SelectParameters's fields from required parameters

#### `ServerSideEncryption`

``` purescript
newtype ServerSideEncryption
  = ServerSideEncryption String
```

##### Instances
``` purescript
Newtype ServerSideEncryption _
Generic ServerSideEncryption _
Show ServerSideEncryption
Decode ServerSideEncryption
Encode ServerSideEncryption
```

#### `ServerSideEncryptionByDefault`

``` purescript
newtype ServerSideEncryptionByDefault
  = ServerSideEncryptionByDefault { "SSEAlgorithm" :: ServerSideEncryption, "KMSMasterKeyID" :: NullOrUndefined (SSEKMSKeyId) }
```

Describes the default server-side encryption to apply to new objects in the bucket. If Put Object request does not specify any server-side encryption, this default encryption will be applied.

##### Instances
``` purescript
Newtype ServerSideEncryptionByDefault _
Generic ServerSideEncryptionByDefault _
Show ServerSideEncryptionByDefault
Decode ServerSideEncryptionByDefault
Encode ServerSideEncryptionByDefault
```

#### `newServerSideEncryptionByDefault`

``` purescript
newServerSideEncryptionByDefault :: ServerSideEncryption -> ServerSideEncryptionByDefault
```

Constructs ServerSideEncryptionByDefault from required parameters

#### `newServerSideEncryptionByDefault'`

``` purescript
newServerSideEncryptionByDefault' :: ServerSideEncryption -> ({ "SSEAlgorithm" :: ServerSideEncryption, "KMSMasterKeyID" :: NullOrUndefined (SSEKMSKeyId) } -> { "SSEAlgorithm" :: ServerSideEncryption, "KMSMasterKeyID" :: NullOrUndefined (SSEKMSKeyId) }) -> ServerSideEncryptionByDefault
```

Constructs ServerSideEncryptionByDefault's fields from required parameters

#### `ServerSideEncryptionConfiguration`

``` purescript
newtype ServerSideEncryptionConfiguration
  = ServerSideEncryptionConfiguration { "Rules" :: ServerSideEncryptionRules }
```

Container for server-side encryption configuration rules. Currently S3 supports one rule only.

##### Instances
``` purescript
Newtype ServerSideEncryptionConfiguration _
Generic ServerSideEncryptionConfiguration _
Show ServerSideEncryptionConfiguration
Decode ServerSideEncryptionConfiguration
Encode ServerSideEncryptionConfiguration
```

#### `newServerSideEncryptionConfiguration`

``` purescript
newServerSideEncryptionConfiguration :: ServerSideEncryptionRules -> ServerSideEncryptionConfiguration
```

Constructs ServerSideEncryptionConfiguration from required parameters

#### `newServerSideEncryptionConfiguration'`

``` purescript
newServerSideEncryptionConfiguration' :: ServerSideEncryptionRules -> ({ "Rules" :: ServerSideEncryptionRules } -> { "Rules" :: ServerSideEncryptionRules }) -> ServerSideEncryptionConfiguration
```

Constructs ServerSideEncryptionConfiguration's fields from required parameters

#### `ServerSideEncryptionRule`

``` purescript
newtype ServerSideEncryptionRule
  = ServerSideEncryptionRule { "ApplyServerSideEncryptionByDefault" :: NullOrUndefined (ServerSideEncryptionByDefault) }
```

Container for information about a particular server-side encryption configuration rule.

##### Instances
``` purescript
Newtype ServerSideEncryptionRule _
Generic ServerSideEncryptionRule _
Show ServerSideEncryptionRule
Decode ServerSideEncryptionRule
Encode ServerSideEncryptionRule
```

#### `newServerSideEncryptionRule`

``` purescript
newServerSideEncryptionRule :: ServerSideEncryptionRule
```

Constructs ServerSideEncryptionRule from required parameters

#### `newServerSideEncryptionRule'`

``` purescript
newServerSideEncryptionRule' :: ({ "ApplyServerSideEncryptionByDefault" :: NullOrUndefined (ServerSideEncryptionByDefault) } -> { "ApplyServerSideEncryptionByDefault" :: NullOrUndefined (ServerSideEncryptionByDefault) }) -> ServerSideEncryptionRule
```

Constructs ServerSideEncryptionRule's fields from required parameters

#### `ServerSideEncryptionRules`

``` purescript
newtype ServerSideEncryptionRules
  = ServerSideEncryptionRules (Array ServerSideEncryptionRule)
```

##### Instances
``` purescript
Newtype ServerSideEncryptionRules _
Generic ServerSideEncryptionRules _
Show ServerSideEncryptionRules
Decode ServerSideEncryptionRules
Encode ServerSideEncryptionRules
```

#### `Size`

``` purescript
newtype Size
  = Size Int
```

##### Instances
``` purescript
Newtype Size _
Generic Size _
Show Size
Decode Size
Encode Size
```

#### `SourceSelectionCriteria`

``` purescript
newtype SourceSelectionCriteria
  = SourceSelectionCriteria { "SseKmsEncryptedObjects" :: NullOrUndefined (SseKmsEncryptedObjects) }
```

Container for filters that define which source objects should be replicated.

##### Instances
``` purescript
Newtype SourceSelectionCriteria _
Generic SourceSelectionCriteria _
Show SourceSelectionCriteria
Decode SourceSelectionCriteria
Encode SourceSelectionCriteria
```

#### `newSourceSelectionCriteria`

``` purescript
newSourceSelectionCriteria :: SourceSelectionCriteria
```

Constructs SourceSelectionCriteria from required parameters

#### `newSourceSelectionCriteria'`

``` purescript
newSourceSelectionCriteria' :: ({ "SseKmsEncryptedObjects" :: NullOrUndefined (SseKmsEncryptedObjects) } -> { "SseKmsEncryptedObjects" :: NullOrUndefined (SseKmsEncryptedObjects) }) -> SourceSelectionCriteria
```

Constructs SourceSelectionCriteria's fields from required parameters

#### `SseKmsEncryptedObjects`

``` purescript
newtype SseKmsEncryptedObjects
  = SseKmsEncryptedObjects { "Status" :: SseKmsEncryptedObjectsStatus }
```

Container for filter information of selection of KMS Encrypted S3 objects.

##### Instances
``` purescript
Newtype SseKmsEncryptedObjects _
Generic SseKmsEncryptedObjects _
Show SseKmsEncryptedObjects
Decode SseKmsEncryptedObjects
Encode SseKmsEncryptedObjects
```

#### `newSseKmsEncryptedObjects`

``` purescript
newSseKmsEncryptedObjects :: SseKmsEncryptedObjectsStatus -> SseKmsEncryptedObjects
```

Constructs SseKmsEncryptedObjects from required parameters

#### `newSseKmsEncryptedObjects'`

``` purescript
newSseKmsEncryptedObjects' :: SseKmsEncryptedObjectsStatus -> ({ "Status" :: SseKmsEncryptedObjectsStatus } -> { "Status" :: SseKmsEncryptedObjectsStatus }) -> SseKmsEncryptedObjects
```

Constructs SseKmsEncryptedObjects's fields from required parameters

#### `SseKmsEncryptedObjectsStatus`

``` purescript
newtype SseKmsEncryptedObjectsStatus
  = SseKmsEncryptedObjectsStatus String
```

##### Instances
``` purescript
Newtype SseKmsEncryptedObjectsStatus _
Generic SseKmsEncryptedObjectsStatus _
Show SseKmsEncryptedObjectsStatus
Decode SseKmsEncryptedObjectsStatus
Encode SseKmsEncryptedObjectsStatus
```

#### `StartAfter`

``` purescript
newtype StartAfter
  = StartAfter String
```

##### Instances
``` purescript
Newtype StartAfter _
Generic StartAfter _
Show StartAfter
Decode StartAfter
Encode StartAfter
```

#### `StorageClass`

``` purescript
newtype StorageClass
  = StorageClass String
```

##### Instances
``` purescript
Newtype StorageClass _
Generic StorageClass _
Show StorageClass
Decode StorageClass
Encode StorageClass
```

#### `StorageClassAnalysis`

``` purescript
newtype StorageClassAnalysis
  = StorageClassAnalysis { "DataExport" :: NullOrUndefined (StorageClassAnalysisDataExport) }
```

##### Instances
``` purescript
Newtype StorageClassAnalysis _
Generic StorageClassAnalysis _
Show StorageClassAnalysis
Decode StorageClassAnalysis
Encode StorageClassAnalysis
```

#### `newStorageClassAnalysis`

``` purescript
newStorageClassAnalysis :: StorageClassAnalysis
```

Constructs StorageClassAnalysis from required parameters

#### `newStorageClassAnalysis'`

``` purescript
newStorageClassAnalysis' :: ({ "DataExport" :: NullOrUndefined (StorageClassAnalysisDataExport) } -> { "DataExport" :: NullOrUndefined (StorageClassAnalysisDataExport) }) -> StorageClassAnalysis
```

Constructs StorageClassAnalysis's fields from required parameters

#### `StorageClassAnalysisDataExport`

``` purescript
newtype StorageClassAnalysisDataExport
  = StorageClassAnalysisDataExport { "OutputSchemaVersion" :: StorageClassAnalysisSchemaVersion, "Destination" :: AnalyticsExportDestination }
```

##### Instances
``` purescript
Newtype StorageClassAnalysisDataExport _
Generic StorageClassAnalysisDataExport _
Show StorageClassAnalysisDataExport
Decode StorageClassAnalysisDataExport
Encode StorageClassAnalysisDataExport
```

#### `newStorageClassAnalysisDataExport`

``` purescript
newStorageClassAnalysisDataExport :: AnalyticsExportDestination -> StorageClassAnalysisSchemaVersion -> StorageClassAnalysisDataExport
```

Constructs StorageClassAnalysisDataExport from required parameters

#### `newStorageClassAnalysisDataExport'`

``` purescript
newStorageClassAnalysisDataExport' :: AnalyticsExportDestination -> StorageClassAnalysisSchemaVersion -> ({ "OutputSchemaVersion" :: StorageClassAnalysisSchemaVersion, "Destination" :: AnalyticsExportDestination } -> { "OutputSchemaVersion" :: StorageClassAnalysisSchemaVersion, "Destination" :: AnalyticsExportDestination }) -> StorageClassAnalysisDataExport
```

Constructs StorageClassAnalysisDataExport's fields from required parameters

#### `StorageClassAnalysisSchemaVersion`

``` purescript
newtype StorageClassAnalysisSchemaVersion
  = StorageClassAnalysisSchemaVersion String
```

##### Instances
``` purescript
Newtype StorageClassAnalysisSchemaVersion _
Generic StorageClassAnalysisSchemaVersion _
Show StorageClassAnalysisSchemaVersion
Decode StorageClassAnalysisSchemaVersion
Encode StorageClassAnalysisSchemaVersion
```

#### `Suffix`

``` purescript
newtype Suffix
  = Suffix String
```

##### Instances
``` purescript
Newtype Suffix _
Generic Suffix _
Show Suffix
Decode Suffix
Encode Suffix
```

#### `Tag`

``` purescript
newtype Tag
  = Tag { "Key" :: ObjectKey, "Value" :: Value }
```

##### Instances
``` purescript
Newtype Tag _
Generic Tag _
Show Tag
Decode Tag
Encode Tag
```

#### `newTag`

``` purescript
newTag :: ObjectKey -> Value -> Tag
```

Constructs Tag from required parameters

#### `newTag'`

``` purescript
newTag' :: ObjectKey -> Value -> ({ "Key" :: ObjectKey, "Value" :: Value } -> { "Key" :: ObjectKey, "Value" :: Value }) -> Tag
```

Constructs Tag's fields from required parameters

#### `TagCount`

``` purescript
newtype TagCount
  = TagCount Int
```

##### Instances
``` purescript
Newtype TagCount _
Generic TagCount _
Show TagCount
Decode TagCount
Encode TagCount
```

#### `TagSet`

``` purescript
newtype TagSet
  = TagSet (Array Tag)
```

##### Instances
``` purescript
Newtype TagSet _
Generic TagSet _
Show TagSet
Decode TagSet
Encode TagSet
```

#### `Tagging`

``` purescript
newtype Tagging
  = Tagging { "TagSet" :: TagSet }
```

##### Instances
``` purescript
Newtype Tagging _
Generic Tagging _
Show Tagging
Decode Tagging
Encode Tagging
```

#### `newTagging`

``` purescript
newTagging :: TagSet -> Tagging
```

Constructs Tagging from required parameters

#### `newTagging'`

``` purescript
newTagging' :: TagSet -> ({ "TagSet" :: TagSet } -> { "TagSet" :: TagSet }) -> Tagging
```

Constructs Tagging's fields from required parameters

#### `TaggingDirective`

``` purescript
newtype TaggingDirective
  = TaggingDirective String
```

##### Instances
``` purescript
Newtype TaggingDirective _
Generic TaggingDirective _
Show TaggingDirective
Decode TaggingDirective
Encode TaggingDirective
```

#### `TaggingHeader`

``` purescript
newtype TaggingHeader
  = TaggingHeader String
```

##### Instances
``` purescript
Newtype TaggingHeader _
Generic TaggingHeader _
Show TaggingHeader
Decode TaggingHeader
Encode TaggingHeader
```

#### `TargetBucket`

``` purescript
newtype TargetBucket
  = TargetBucket String
```

##### Instances
``` purescript
Newtype TargetBucket _
Generic TargetBucket _
Show TargetBucket
Decode TargetBucket
Encode TargetBucket
```

#### `TargetGrant`

``` purescript
newtype TargetGrant
  = TargetGrant { "Grantee" :: NullOrUndefined (Grantee), "Permission" :: NullOrUndefined (BucketLogsPermission) }
```

##### Instances
``` purescript
Newtype TargetGrant _
Generic TargetGrant _
Show TargetGrant
Decode TargetGrant
Encode TargetGrant
```

#### `newTargetGrant`

``` purescript
newTargetGrant :: TargetGrant
```

Constructs TargetGrant from required parameters

#### `newTargetGrant'`

``` purescript
newTargetGrant' :: ({ "Grantee" :: NullOrUndefined (Grantee), "Permission" :: NullOrUndefined (BucketLogsPermission) } -> { "Grantee" :: NullOrUndefined (Grantee), "Permission" :: NullOrUndefined (BucketLogsPermission) }) -> TargetGrant
```

Constructs TargetGrant's fields from required parameters

#### `TargetGrants`

``` purescript
newtype TargetGrants
  = TargetGrants (Array TargetGrant)
```

##### Instances
``` purescript
Newtype TargetGrants _
Generic TargetGrants _
Show TargetGrants
Decode TargetGrants
Encode TargetGrants
```

#### `TargetPrefix`

``` purescript
newtype TargetPrefix
  = TargetPrefix String
```

##### Instances
``` purescript
Newtype TargetPrefix _
Generic TargetPrefix _
Show TargetPrefix
Decode TargetPrefix
Encode TargetPrefix
```

#### `Tier`

``` purescript
newtype Tier
  = Tier String
```

##### Instances
``` purescript
Newtype Tier _
Generic Tier _
Show Tier
Decode Tier
Encode Tier
```

#### `Token`

``` purescript
newtype Token
  = Token String
```

##### Instances
``` purescript
Newtype Token _
Generic Token _
Show Token
Decode Token
Encode Token
```

#### `TopicArn`

``` purescript
newtype TopicArn
  = TopicArn String
```

##### Instances
``` purescript
Newtype TopicArn _
Generic TopicArn _
Show TopicArn
Decode TopicArn
Encode TopicArn
```

#### `TopicConfiguration`

``` purescript
newtype TopicConfiguration
  = TopicConfiguration { "Id" :: NullOrUndefined (NotificationId), "TopicArn" :: TopicArn, "Events" :: EventList, "Filter" :: NullOrUndefined (NotificationConfigurationFilter) }
```

Container for specifying the configuration when you want Amazon S3 to publish events to an Amazon Simple Notification Service (Amazon SNS) topic.

##### Instances
``` purescript
Newtype TopicConfiguration _
Generic TopicConfiguration _
Show TopicConfiguration
Decode TopicConfiguration
Encode TopicConfiguration
```

#### `newTopicConfiguration`

``` purescript
newTopicConfiguration :: EventList -> TopicArn -> TopicConfiguration
```

Constructs TopicConfiguration from required parameters

#### `newTopicConfiguration'`

``` purescript
newTopicConfiguration' :: EventList -> TopicArn -> ({ "Id" :: NullOrUndefined (NotificationId), "TopicArn" :: TopicArn, "Events" :: EventList, "Filter" :: NullOrUndefined (NotificationConfigurationFilter) } -> { "Id" :: NullOrUndefined (NotificationId), "TopicArn" :: TopicArn, "Events" :: EventList, "Filter" :: NullOrUndefined (NotificationConfigurationFilter) }) -> TopicConfiguration
```

Constructs TopicConfiguration's fields from required parameters

#### `TopicConfigurationDeprecated`

``` purescript
newtype TopicConfigurationDeprecated
  = TopicConfigurationDeprecated { "Id" :: NullOrUndefined (NotificationId), "Events" :: NullOrUndefined (EventList), "Event" :: NullOrUndefined (Event), "Topic" :: NullOrUndefined (TopicArn) }
```

##### Instances
``` purescript
Newtype TopicConfigurationDeprecated _
Generic TopicConfigurationDeprecated _
Show TopicConfigurationDeprecated
Decode TopicConfigurationDeprecated
Encode TopicConfigurationDeprecated
```

#### `newTopicConfigurationDeprecated`

``` purescript
newTopicConfigurationDeprecated :: TopicConfigurationDeprecated
```

Constructs TopicConfigurationDeprecated from required parameters

#### `newTopicConfigurationDeprecated'`

``` purescript
newTopicConfigurationDeprecated' :: ({ "Id" :: NullOrUndefined (NotificationId), "Events" :: NullOrUndefined (EventList), "Event" :: NullOrUndefined (Event), "Topic" :: NullOrUndefined (TopicArn) } -> { "Id" :: NullOrUndefined (NotificationId), "Events" :: NullOrUndefined (EventList), "Event" :: NullOrUndefined (Event), "Topic" :: NullOrUndefined (TopicArn) }) -> TopicConfigurationDeprecated
```

Constructs TopicConfigurationDeprecated's fields from required parameters

#### `TopicConfigurationList`

``` purescript
newtype TopicConfigurationList
  = TopicConfigurationList (Array TopicConfiguration)
```

##### Instances
``` purescript
Newtype TopicConfigurationList _
Generic TopicConfigurationList _
Show TopicConfigurationList
Decode TopicConfigurationList
Encode TopicConfigurationList
```

#### `Transition`

``` purescript
newtype Transition
  = Transition { "Date" :: NullOrUndefined (Date), "Days" :: NullOrUndefined (Days), "StorageClass" :: NullOrUndefined (TransitionStorageClass) }
```

##### Instances
``` purescript
Newtype Transition _
Generic Transition _
Show Transition
Decode Transition
Encode Transition
```

#### `newTransition`

``` purescript
newTransition :: Transition
```

Constructs Transition from required parameters

#### `newTransition'`

``` purescript
newTransition' :: ({ "Date" :: NullOrUndefined (Date), "Days" :: NullOrUndefined (Days), "StorageClass" :: NullOrUndefined (TransitionStorageClass) } -> { "Date" :: NullOrUndefined (Date), "Days" :: NullOrUndefined (Days), "StorageClass" :: NullOrUndefined (TransitionStorageClass) }) -> Transition
```

Constructs Transition's fields from required parameters

#### `TransitionList`

``` purescript
newtype TransitionList
  = TransitionList (Array Transition)
```

##### Instances
``` purescript
Newtype TransitionList _
Generic TransitionList _
Show TransitionList
Decode TransitionList
Encode TransitionList
```

#### `TransitionStorageClass`

``` purescript
newtype TransitionStorageClass
  = TransitionStorageClass String
```

##### Instances
``` purescript
Newtype TransitionStorageClass _
Generic TransitionStorageClass _
Show TransitionStorageClass
Decode TransitionStorageClass
Encode TransitionStorageClass
```

#### `Type`

``` purescript
newtype Type
  = Type String
```

##### Instances
``` purescript
Newtype Type _
Generic Type _
Show Type
Decode Type
Encode Type
```

#### `URI`

``` purescript
newtype URI
  = URI String
```

##### Instances
``` purescript
Newtype URI _
Generic URI _
Show URI
Decode URI
Encode URI
```

#### `UploadIdMarker`

``` purescript
newtype UploadIdMarker
  = UploadIdMarker String
```

##### Instances
``` purescript
Newtype UploadIdMarker _
Generic UploadIdMarker _
Show UploadIdMarker
Decode UploadIdMarker
Encode UploadIdMarker
```

#### `UploadPartCopyOutput`

``` purescript
newtype UploadPartCopyOutput
  = UploadPartCopyOutput { "CopySourceVersionId" :: NullOrUndefined (CopySourceVersionId), "CopyPartResult" :: NullOrUndefined (CopyPartResult), "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption), "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm), "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5), "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId), "RequestCharged" :: NullOrUndefined (RequestCharged) }
```

##### Instances
``` purescript
Newtype UploadPartCopyOutput _
Generic UploadPartCopyOutput _
Show UploadPartCopyOutput
Decode UploadPartCopyOutput
Encode UploadPartCopyOutput
```

#### `newUploadPartCopyOutput`

``` purescript
newUploadPartCopyOutput :: UploadPartCopyOutput
```

Constructs UploadPartCopyOutput from required parameters

#### `newUploadPartCopyOutput'`

``` purescript
newUploadPartCopyOutput' :: ({ "CopySourceVersionId" :: NullOrUndefined (CopySourceVersionId), "CopyPartResult" :: NullOrUndefined (CopyPartResult), "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption), "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm), "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5), "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId), "RequestCharged" :: NullOrUndefined (RequestCharged) } -> { "CopySourceVersionId" :: NullOrUndefined (CopySourceVersionId), "CopyPartResult" :: NullOrUndefined (CopyPartResult), "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption), "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm), "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5), "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId), "RequestCharged" :: NullOrUndefined (RequestCharged) }) -> UploadPartCopyOutput
```

Constructs UploadPartCopyOutput's fields from required parameters

#### `UploadPartCopyRequest`

``` purescript
newtype UploadPartCopyRequest
  = UploadPartCopyRequest { "Bucket" :: BucketName, "CopySource" :: CopySource, "CopySourceIfMatch" :: NullOrUndefined (CopySourceIfMatch), "CopySourceIfModifiedSince" :: NullOrUndefined (CopySourceIfModifiedSince), "CopySourceIfNoneMatch" :: NullOrUndefined (CopySourceIfNoneMatch), "CopySourceIfUnmodifiedSince" :: NullOrUndefined (CopySourceIfUnmodifiedSince), "CopySourceRange" :: NullOrUndefined (CopySourceRange), "Key" :: ObjectKey, "PartNumber" :: PartNumber, "UploadId" :: MultipartUploadId, "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm), "SSECustomerKey" :: NullOrUndefined (SSECustomerKey), "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5), "CopySourceSSECustomerAlgorithm" :: NullOrUndefined (CopySourceSSECustomerAlgorithm), "CopySourceSSECustomerKey" :: NullOrUndefined (CopySourceSSECustomerKey), "CopySourceSSECustomerKeyMD5" :: NullOrUndefined (CopySourceSSECustomerKeyMD5), "RequestPayer" :: NullOrUndefined (RequestPayer) }
```

##### Instances
``` purescript
Newtype UploadPartCopyRequest _
Generic UploadPartCopyRequest _
Show UploadPartCopyRequest
Decode UploadPartCopyRequest
Encode UploadPartCopyRequest
```

#### `newUploadPartCopyRequest`

``` purescript
newUploadPartCopyRequest :: BucketName -> CopySource -> ObjectKey -> PartNumber -> MultipartUploadId -> UploadPartCopyRequest
```

Constructs UploadPartCopyRequest from required parameters

#### `newUploadPartCopyRequest'`

``` purescript
newUploadPartCopyRequest' :: BucketName -> CopySource -> ObjectKey -> PartNumber -> MultipartUploadId -> ({ "Bucket" :: BucketName, "CopySource" :: CopySource, "CopySourceIfMatch" :: NullOrUndefined (CopySourceIfMatch), "CopySourceIfModifiedSince" :: NullOrUndefined (CopySourceIfModifiedSince), "CopySourceIfNoneMatch" :: NullOrUndefined (CopySourceIfNoneMatch), "CopySourceIfUnmodifiedSince" :: NullOrUndefined (CopySourceIfUnmodifiedSince), "CopySourceRange" :: NullOrUndefined (CopySourceRange), "Key" :: ObjectKey, "PartNumber" :: PartNumber, "UploadId" :: MultipartUploadId, "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm), "SSECustomerKey" :: NullOrUndefined (SSECustomerKey), "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5), "CopySourceSSECustomerAlgorithm" :: NullOrUndefined (CopySourceSSECustomerAlgorithm), "CopySourceSSECustomerKey" :: NullOrUndefined (CopySourceSSECustomerKey), "CopySourceSSECustomerKeyMD5" :: NullOrUndefined (CopySourceSSECustomerKeyMD5), "RequestPayer" :: NullOrUndefined (RequestPayer) } -> { "Bucket" :: BucketName, "CopySource" :: CopySource, "CopySourceIfMatch" :: NullOrUndefined (CopySourceIfMatch), "CopySourceIfModifiedSince" :: NullOrUndefined (CopySourceIfModifiedSince), "CopySourceIfNoneMatch" :: NullOrUndefined (CopySourceIfNoneMatch), "CopySourceIfUnmodifiedSince" :: NullOrUndefined (CopySourceIfUnmodifiedSince), "CopySourceRange" :: NullOrUndefined (CopySourceRange), "Key" :: ObjectKey, "PartNumber" :: PartNumber, "UploadId" :: MultipartUploadId, "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm), "SSECustomerKey" :: NullOrUndefined (SSECustomerKey), "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5), "CopySourceSSECustomerAlgorithm" :: NullOrUndefined (CopySourceSSECustomerAlgorithm), "CopySourceSSECustomerKey" :: NullOrUndefined (CopySourceSSECustomerKey), "CopySourceSSECustomerKeyMD5" :: NullOrUndefined (CopySourceSSECustomerKeyMD5), "RequestPayer" :: NullOrUndefined (RequestPayer) }) -> UploadPartCopyRequest
```

Constructs UploadPartCopyRequest's fields from required parameters

#### `UploadPartOutput`

``` purescript
newtype UploadPartOutput
  = UploadPartOutput { "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption), "ETag" :: NullOrUndefined (ETag), "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm), "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5), "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId), "RequestCharged" :: NullOrUndefined (RequestCharged) }
```

##### Instances
``` purescript
Newtype UploadPartOutput _
Generic UploadPartOutput _
Show UploadPartOutput
Decode UploadPartOutput
Encode UploadPartOutput
```

#### `newUploadPartOutput`

``` purescript
newUploadPartOutput :: UploadPartOutput
```

Constructs UploadPartOutput from required parameters

#### `newUploadPartOutput'`

``` purescript
newUploadPartOutput' :: ({ "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption), "ETag" :: NullOrUndefined (ETag), "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm), "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5), "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId), "RequestCharged" :: NullOrUndefined (RequestCharged) } -> { "ServerSideEncryption" :: NullOrUndefined (ServerSideEncryption), "ETag" :: NullOrUndefined (ETag), "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm), "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5), "SSEKMSKeyId" :: NullOrUndefined (SSEKMSKeyId), "RequestCharged" :: NullOrUndefined (RequestCharged) }) -> UploadPartOutput
```

Constructs UploadPartOutput's fields from required parameters

#### `UploadPartRequest`

``` purescript
newtype UploadPartRequest
  = UploadPartRequest { "Body" :: NullOrUndefined (Body), "Bucket" :: BucketName, "ContentLength" :: NullOrUndefined (ContentLength), "ContentMD5" :: NullOrUndefined (ContentMD5), "Key" :: ObjectKey, "PartNumber" :: PartNumber, "UploadId" :: MultipartUploadId, "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm), "SSECustomerKey" :: NullOrUndefined (SSECustomerKey), "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5), "RequestPayer" :: NullOrUndefined (RequestPayer) }
```

##### Instances
``` purescript
Newtype UploadPartRequest _
Generic UploadPartRequest _
Show UploadPartRequest
Decode UploadPartRequest
Encode UploadPartRequest
```

#### `newUploadPartRequest`

``` purescript
newUploadPartRequest :: BucketName -> ObjectKey -> PartNumber -> MultipartUploadId -> UploadPartRequest
```

Constructs UploadPartRequest from required parameters

#### `newUploadPartRequest'`

``` purescript
newUploadPartRequest' :: BucketName -> ObjectKey -> PartNumber -> MultipartUploadId -> ({ "Body" :: NullOrUndefined (Body), "Bucket" :: BucketName, "ContentLength" :: NullOrUndefined (ContentLength), "ContentMD5" :: NullOrUndefined (ContentMD5), "Key" :: ObjectKey, "PartNumber" :: PartNumber, "UploadId" :: MultipartUploadId, "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm), "SSECustomerKey" :: NullOrUndefined (SSECustomerKey), "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5), "RequestPayer" :: NullOrUndefined (RequestPayer) } -> { "Body" :: NullOrUndefined (Body), "Bucket" :: BucketName, "ContentLength" :: NullOrUndefined (ContentLength), "ContentMD5" :: NullOrUndefined (ContentMD5), "Key" :: ObjectKey, "PartNumber" :: PartNumber, "UploadId" :: MultipartUploadId, "SSECustomerAlgorithm" :: NullOrUndefined (SSECustomerAlgorithm), "SSECustomerKey" :: NullOrUndefined (SSECustomerKey), "SSECustomerKeyMD5" :: NullOrUndefined (SSECustomerKeyMD5), "RequestPayer" :: NullOrUndefined (RequestPayer) }) -> UploadPartRequest
```

Constructs UploadPartRequest's fields from required parameters

#### `UserMetadata`

``` purescript
newtype UserMetadata
  = UserMetadata (Array MetadataEntry)
```

##### Instances
``` purescript
Newtype UserMetadata _
Generic UserMetadata _
Show UserMetadata
Decode UserMetadata
Encode UserMetadata
```

#### `Value`

``` purescript
newtype Value
  = Value String
```

##### Instances
``` purescript
Newtype Value _
Generic Value _
Show Value
Decode Value
Encode Value
```

#### `VersionIdMarker`

``` purescript
newtype VersionIdMarker
  = VersionIdMarker String
```

##### Instances
``` purescript
Newtype VersionIdMarker _
Generic VersionIdMarker _
Show VersionIdMarker
Decode VersionIdMarker
Encode VersionIdMarker
```

#### `VersioningConfiguration`

``` purescript
newtype VersioningConfiguration
  = VersioningConfiguration { "MFADelete" :: NullOrUndefined (MFADelete), "Status" :: NullOrUndefined (BucketVersioningStatus) }
```

##### Instances
``` purescript
Newtype VersioningConfiguration _
Generic VersioningConfiguration _
Show VersioningConfiguration
Decode VersioningConfiguration
Encode VersioningConfiguration
```

#### `newVersioningConfiguration`

``` purescript
newVersioningConfiguration :: VersioningConfiguration
```

Constructs VersioningConfiguration from required parameters

#### `newVersioningConfiguration'`

``` purescript
newVersioningConfiguration' :: ({ "MFADelete" :: NullOrUndefined (MFADelete), "Status" :: NullOrUndefined (BucketVersioningStatus) } -> { "MFADelete" :: NullOrUndefined (MFADelete), "Status" :: NullOrUndefined (BucketVersioningStatus) }) -> VersioningConfiguration
```

Constructs VersioningConfiguration's fields from required parameters

#### `WebsiteConfiguration`

``` purescript
newtype WebsiteConfiguration
  = WebsiteConfiguration { "ErrorDocument" :: NullOrUndefined (ErrorDocument), "IndexDocument" :: NullOrUndefined (IndexDocument), "RedirectAllRequestsTo" :: NullOrUndefined (RedirectAllRequestsTo), "RoutingRules" :: NullOrUndefined (RoutingRules) }
```

##### Instances
``` purescript
Newtype WebsiteConfiguration _
Generic WebsiteConfiguration _
Show WebsiteConfiguration
Decode WebsiteConfiguration
Encode WebsiteConfiguration
```

#### `newWebsiteConfiguration`

``` purescript
newWebsiteConfiguration :: WebsiteConfiguration
```

Constructs WebsiteConfiguration from required parameters

#### `newWebsiteConfiguration'`

``` purescript
newWebsiteConfiguration' :: ({ "ErrorDocument" :: NullOrUndefined (ErrorDocument), "IndexDocument" :: NullOrUndefined (IndexDocument), "RedirectAllRequestsTo" :: NullOrUndefined (RedirectAllRequestsTo), "RoutingRules" :: NullOrUndefined (RoutingRules) } -> { "ErrorDocument" :: NullOrUndefined (ErrorDocument), "IndexDocument" :: NullOrUndefined (IndexDocument), "RedirectAllRequestsTo" :: NullOrUndefined (RedirectAllRequestsTo), "RoutingRules" :: NullOrUndefined (RoutingRules) }) -> WebsiteConfiguration
```

Constructs WebsiteConfiguration's fields from required parameters

#### `WebsiteRedirectLocation`

``` purescript
newtype WebsiteRedirectLocation
  = WebsiteRedirectLocation String
```

##### Instances
``` purescript
Newtype WebsiteRedirectLocation _
Generic WebsiteRedirectLocation _
Show WebsiteRedirectLocation
Decode WebsiteRedirectLocation
Encode WebsiteRedirectLocation
```


