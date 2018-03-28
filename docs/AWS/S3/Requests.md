## Module AWS.S3.Requests

#### `abortMultipartUpload`

``` purescript
abortMultipartUpload :: forall eff. Service -> AbortMultipartUploadRequest -> Aff (exception :: EXCEPTION | eff) AbortMultipartUploadOutput
```

<p>Aborts a multipart upload.</p><p>To verify that all parts have been removed, so you don't get charged for the part storage, you should call the List Parts operation and ensure the parts list is empty.</p>

#### `completeMultipartUpload`

``` purescript
completeMultipartUpload :: forall eff. Service -> CompleteMultipartUploadRequest -> Aff (exception :: EXCEPTION | eff) CompleteMultipartUploadOutput
```

Completes a multipart upload by assembling previously uploaded parts.

#### `copyObject`

``` purescript
copyObject :: forall eff. Service -> CopyObjectRequest -> Aff (exception :: EXCEPTION | eff) CopyObjectOutput
```

Creates a copy of an object that is already stored in Amazon S3.

#### `createBucket`

``` purescript
createBucket :: forall eff. Service -> CreateBucketRequest -> Aff (exception :: EXCEPTION | eff) CreateBucketOutput
```

Creates a new bucket.

#### `createMultipartUpload`

``` purescript
createMultipartUpload :: forall eff. Service -> CreateMultipartUploadRequest -> Aff (exception :: EXCEPTION | eff) CreateMultipartUploadOutput
```

<p>Initiates a multipart upload and returns an upload ID.</p><p><b>Note:</b> After you initiate multipart upload and upload one or more parts, you must either complete or abort multipart upload in order to stop getting charged for storage of the uploaded parts. Only after you either complete or abort multipart upload, Amazon S3 frees up the parts storage and stops charging you for the parts storage.</p>

#### `deleteBucket`

``` purescript
deleteBucket :: forall eff. Service -> DeleteBucketRequest -> Aff (exception :: EXCEPTION | eff) NoOutput
```

Deletes the bucket. All objects (including all object versions and Delete Markers) in the bucket must be deleted before the bucket itself can be deleted.

#### `deleteBucketAnalyticsConfiguration`

``` purescript
deleteBucketAnalyticsConfiguration :: forall eff. Service -> DeleteBucketAnalyticsConfigurationRequest -> Aff (exception :: EXCEPTION | eff) NoOutput
```

Deletes an analytics configuration for the bucket (specified by the analytics configuration ID).

#### `deleteBucketCors`

``` purescript
deleteBucketCors :: forall eff. Service -> DeleteBucketCorsRequest -> Aff (exception :: EXCEPTION | eff) NoOutput
```

Deletes the cors configuration information set for the bucket.

#### `deleteBucketEncryption`

``` purescript
deleteBucketEncryption :: forall eff. Service -> DeleteBucketEncryptionRequest -> Aff (exception :: EXCEPTION | eff) NoOutput
```

Deletes the server-side encryption configuration from the bucket.

#### `deleteBucketInventoryConfiguration`

``` purescript
deleteBucketInventoryConfiguration :: forall eff. Service -> DeleteBucketInventoryConfigurationRequest -> Aff (exception :: EXCEPTION | eff) NoOutput
```

Deletes an inventory configuration (identified by the inventory ID) from the bucket.

#### `deleteBucketLifecycle`

``` purescript
deleteBucketLifecycle :: forall eff. Service -> DeleteBucketLifecycleRequest -> Aff (exception :: EXCEPTION | eff) NoOutput
```

Deletes the lifecycle configuration from the bucket.

#### `deleteBucketMetricsConfiguration`

``` purescript
deleteBucketMetricsConfiguration :: forall eff. Service -> DeleteBucketMetricsConfigurationRequest -> Aff (exception :: EXCEPTION | eff) NoOutput
```

Deletes a metrics configuration (specified by the metrics configuration ID) from the bucket.

#### `deleteBucketPolicy`

``` purescript
deleteBucketPolicy :: forall eff. Service -> DeleteBucketPolicyRequest -> Aff (exception :: EXCEPTION | eff) NoOutput
```

Deletes the policy from the bucket.

#### `deleteBucketReplication`

``` purescript
deleteBucketReplication :: forall eff. Service -> DeleteBucketReplicationRequest -> Aff (exception :: EXCEPTION | eff) NoOutput
```

Deletes the replication configuration from the bucket.

#### `deleteBucketTagging`

``` purescript
deleteBucketTagging :: forall eff. Service -> DeleteBucketTaggingRequest -> Aff (exception :: EXCEPTION | eff) NoOutput
```

Deletes the tags from the bucket.

#### `deleteBucketWebsite`

``` purescript
deleteBucketWebsite :: forall eff. Service -> DeleteBucketWebsiteRequest -> Aff (exception :: EXCEPTION | eff) NoOutput
```

This operation removes the website configuration from the bucket.

#### `deleteObject`

``` purescript
deleteObject :: forall eff. Service -> DeleteObjectRequest -> Aff (exception :: EXCEPTION | eff) DeleteObjectOutput
```

Removes the null version (if there is one) of an object and inserts a delete marker, which becomes the latest version of the object. If there isn't a null version, Amazon S3 does not remove any objects.

#### `deleteObjectTagging`

``` purescript
deleteObjectTagging :: forall eff. Service -> DeleteObjectTaggingRequest -> Aff (exception :: EXCEPTION | eff) DeleteObjectTaggingOutput
```

Removes the tag-set from an existing object.

#### `deleteObjects`

``` purescript
deleteObjects :: forall eff. Service -> DeleteObjectsRequest -> Aff (exception :: EXCEPTION | eff) DeleteObjectsOutput
```

This operation enables you to delete multiple objects from a bucket using a single HTTP request. You may specify up to 1000 keys.

#### `getBucketAccelerateConfiguration`

``` purescript
getBucketAccelerateConfiguration :: forall eff. Service -> GetBucketAccelerateConfigurationRequest -> Aff (exception :: EXCEPTION | eff) GetBucketAccelerateConfigurationOutput
```

Returns the accelerate configuration of a bucket.

#### `getBucketAcl`

``` purescript
getBucketAcl :: forall eff. Service -> GetBucketAclRequest -> Aff (exception :: EXCEPTION | eff) GetBucketAclOutput
```

Gets the access control policy for the bucket.

#### `getBucketAnalyticsConfiguration`

``` purescript
getBucketAnalyticsConfiguration :: forall eff. Service -> GetBucketAnalyticsConfigurationRequest -> Aff (exception :: EXCEPTION | eff) GetBucketAnalyticsConfigurationOutput
```

Gets an analytics configuration for the bucket (specified by the analytics configuration ID).

#### `getBucketCors`

``` purescript
getBucketCors :: forall eff. Service -> GetBucketCorsRequest -> Aff (exception :: EXCEPTION | eff) GetBucketCorsOutput
```

Returns the cors configuration for the bucket.

#### `getBucketEncryption`

``` purescript
getBucketEncryption :: forall eff. Service -> GetBucketEncryptionRequest -> Aff (exception :: EXCEPTION | eff) GetBucketEncryptionOutput
```

Returns the server-side encryption configuration of a bucket.

#### `getBucketInventoryConfiguration`

``` purescript
getBucketInventoryConfiguration :: forall eff. Service -> GetBucketInventoryConfigurationRequest -> Aff (exception :: EXCEPTION | eff) GetBucketInventoryConfigurationOutput
```

Returns an inventory configuration (identified by the inventory ID) from the bucket.

#### `getBucketLifecycle`

``` purescript
getBucketLifecycle :: forall eff. Service -> GetBucketLifecycleRequest -> Aff (exception :: EXCEPTION | eff) GetBucketLifecycleOutput
```

Deprecated, see the GetBucketLifecycleConfiguration operation.

#### `getBucketLifecycleConfiguration`

``` purescript
getBucketLifecycleConfiguration :: forall eff. Service -> GetBucketLifecycleConfigurationRequest -> Aff (exception :: EXCEPTION | eff) GetBucketLifecycleConfigurationOutput
```

Returns the lifecycle configuration information set on the bucket.

#### `getBucketLocation`

``` purescript
getBucketLocation :: forall eff. Service -> GetBucketLocationRequest -> Aff (exception :: EXCEPTION | eff) GetBucketLocationOutput
```

Returns the region the bucket resides in.

#### `getBucketLogging`

``` purescript
getBucketLogging :: forall eff. Service -> GetBucketLoggingRequest -> Aff (exception :: EXCEPTION | eff) GetBucketLoggingOutput
```

Returns the logging status of a bucket and the permissions users have to view and modify that status. To use GET, you must be the bucket owner.

#### `getBucketMetricsConfiguration`

``` purescript
getBucketMetricsConfiguration :: forall eff. Service -> GetBucketMetricsConfigurationRequest -> Aff (exception :: EXCEPTION | eff) GetBucketMetricsConfigurationOutput
```

Gets a metrics configuration (specified by the metrics configuration ID) from the bucket.

#### `getBucketNotification`

``` purescript
getBucketNotification :: forall eff. Service -> GetBucketNotificationConfigurationRequest -> Aff (exception :: EXCEPTION | eff) NotificationConfigurationDeprecated
```

Deprecated, see the GetBucketNotificationConfiguration operation.

#### `getBucketNotificationConfiguration`

``` purescript
getBucketNotificationConfiguration :: forall eff. Service -> GetBucketNotificationConfigurationRequest -> Aff (exception :: EXCEPTION | eff) NotificationConfiguration
```

Returns the notification configuration of a bucket.

#### `getBucketPolicy`

``` purescript
getBucketPolicy :: forall eff. Service -> GetBucketPolicyRequest -> Aff (exception :: EXCEPTION | eff) GetBucketPolicyOutput
```

Returns the policy of a specified bucket.

#### `getBucketReplication`

``` purescript
getBucketReplication :: forall eff. Service -> GetBucketReplicationRequest -> Aff (exception :: EXCEPTION | eff) GetBucketReplicationOutput
```

Returns the replication configuration of a bucket.

#### `getBucketRequestPayment`

``` purescript
getBucketRequestPayment :: forall eff. Service -> GetBucketRequestPaymentRequest -> Aff (exception :: EXCEPTION | eff) GetBucketRequestPaymentOutput
```

Returns the request payment configuration of a bucket.

#### `getBucketTagging`

``` purescript
getBucketTagging :: forall eff. Service -> GetBucketTaggingRequest -> Aff (exception :: EXCEPTION | eff) GetBucketTaggingOutput
```

Returns the tag set associated with the bucket.

#### `getBucketVersioning`

``` purescript
getBucketVersioning :: forall eff. Service -> GetBucketVersioningRequest -> Aff (exception :: EXCEPTION | eff) GetBucketVersioningOutput
```

Returns the versioning state of a bucket.

#### `getBucketWebsite`

``` purescript
getBucketWebsite :: forall eff. Service -> GetBucketWebsiteRequest -> Aff (exception :: EXCEPTION | eff) GetBucketWebsiteOutput
```

Returns the website configuration for a bucket.

#### `getObject`

``` purescript
getObject :: forall eff. Service -> GetObjectRequest -> Aff (exception :: EXCEPTION | eff) GetObjectOutput
```

Retrieves objects from Amazon S3.

#### `getObjectAcl`

``` purescript
getObjectAcl :: forall eff. Service -> GetObjectAclRequest -> Aff (exception :: EXCEPTION | eff) GetObjectAclOutput
```

Returns the access control list (ACL) of an object.

#### `getObjectTagging`

``` purescript
getObjectTagging :: forall eff. Service -> GetObjectTaggingRequest -> Aff (exception :: EXCEPTION | eff) GetObjectTaggingOutput
```

Returns the tag-set of an object.

#### `getObjectTorrent`

``` purescript
getObjectTorrent :: forall eff. Service -> GetObjectTorrentRequest -> Aff (exception :: EXCEPTION | eff) GetObjectTorrentOutput
```

Return torrent files from a bucket.

#### `headBucket`

``` purescript
headBucket :: forall eff. Service -> HeadBucketRequest -> Aff (exception :: EXCEPTION | eff) NoOutput
```

This operation is useful to determine if a bucket exists and you have permission to access it.

#### `headObject`

``` purescript
headObject :: forall eff. Service -> HeadObjectRequest -> Aff (exception :: EXCEPTION | eff) HeadObjectOutput
```

The HEAD operation retrieves metadata from an object without returning the object itself. This operation is useful if you're only interested in an object's metadata. To use HEAD, you must have READ access to the object.

#### `listBucketAnalyticsConfigurations`

``` purescript
listBucketAnalyticsConfigurations :: forall eff. Service -> ListBucketAnalyticsConfigurationsRequest -> Aff (exception :: EXCEPTION | eff) ListBucketAnalyticsConfigurationsOutput
```

Lists the analytics configurations for the bucket.

#### `listBucketInventoryConfigurations`

``` purescript
listBucketInventoryConfigurations :: forall eff. Service -> ListBucketInventoryConfigurationsRequest -> Aff (exception :: EXCEPTION | eff) ListBucketInventoryConfigurationsOutput
```

Returns a list of inventory configurations for the bucket.

#### `listBucketMetricsConfigurations`

``` purescript
listBucketMetricsConfigurations :: forall eff. Service -> ListBucketMetricsConfigurationsRequest -> Aff (exception :: EXCEPTION | eff) ListBucketMetricsConfigurationsOutput
```

Lists the metrics configurations for the bucket.

#### `listBuckets`

``` purescript
listBuckets :: forall eff. Service -> Aff (exception :: EXCEPTION | eff) ListBucketsOutput
```

Returns a list of all buckets owned by the authenticated sender of the request.

#### `listMultipartUploads`

``` purescript
listMultipartUploads :: forall eff. Service -> ListMultipartUploadsRequest -> Aff (exception :: EXCEPTION | eff) ListMultipartUploadsOutput
```

This operation lists in-progress multipart uploads.

#### `listObjectVersions`

``` purescript
listObjectVersions :: forall eff. Service -> ListObjectVersionsRequest -> Aff (exception :: EXCEPTION | eff) ListObjectVersionsOutput
```

Returns metadata about all of the versions of objects in a bucket.

#### `listObjects`

``` purescript
listObjects :: forall eff. Service -> ListObjectsRequest -> Aff (exception :: EXCEPTION | eff) ListObjectsOutput
```

Returns some or all (up to 1000) of the objects in a bucket. You can use the request parameters as selection criteria to return a subset of the objects in a bucket.

#### `listObjectsV2`

``` purescript
listObjectsV2 :: forall eff. Service -> ListObjectsV2Request -> Aff (exception :: EXCEPTION | eff) ListObjectsV2Output
```

Returns some or all (up to 1000) of the objects in a bucket. You can use the request parameters as selection criteria to return a subset of the objects in a bucket. Note: ListObjectsV2 is the revised List Objects API and we recommend you use this revised API for new application development.

#### `listParts`

``` purescript
listParts :: forall eff. Service -> ListPartsRequest -> Aff (exception :: EXCEPTION | eff) ListPartsOutput
```

Lists the parts that have been uploaded for a specific multipart upload.

#### `putBucketAccelerateConfiguration`

``` purescript
putBucketAccelerateConfiguration :: forall eff. Service -> PutBucketAccelerateConfigurationRequest -> Aff (exception :: EXCEPTION | eff) NoOutput
```

Sets the accelerate configuration of an existing bucket.

#### `putBucketAcl`

``` purescript
putBucketAcl :: forall eff. Service -> PutBucketAclRequest -> Aff (exception :: EXCEPTION | eff) NoOutput
```

Sets the permissions on a bucket using access control lists (ACL).

#### `putBucketAnalyticsConfiguration`

``` purescript
putBucketAnalyticsConfiguration :: forall eff. Service -> PutBucketAnalyticsConfigurationRequest -> Aff (exception :: EXCEPTION | eff) NoOutput
```

Sets an analytics configuration for the bucket (specified by the analytics configuration ID).

#### `putBucketCors`

``` purescript
putBucketCors :: forall eff. Service -> PutBucketCorsRequest -> Aff (exception :: EXCEPTION | eff) NoOutput
```

Sets the cors configuration for a bucket.

#### `putBucketEncryption`

``` purescript
putBucketEncryption :: forall eff. Service -> PutBucketEncryptionRequest -> Aff (exception :: EXCEPTION | eff) NoOutput
```

Creates a new server-side encryption configuration (or replaces an existing one, if present).

#### `putBucketInventoryConfiguration`

``` purescript
putBucketInventoryConfiguration :: forall eff. Service -> PutBucketInventoryConfigurationRequest -> Aff (exception :: EXCEPTION | eff) NoOutput
```

Adds an inventory configuration (identified by the inventory ID) from the bucket.

#### `putBucketLifecycle`

``` purescript
putBucketLifecycle :: forall eff. Service -> PutBucketLifecycleRequest -> Aff (exception :: EXCEPTION | eff) NoOutput
```

Deprecated, see the PutBucketLifecycleConfiguration operation.

#### `putBucketLifecycleConfiguration`

``` purescript
putBucketLifecycleConfiguration :: forall eff. Service -> PutBucketLifecycleConfigurationRequest -> Aff (exception :: EXCEPTION | eff) NoOutput
```

Sets lifecycle configuration for your bucket. If a lifecycle configuration exists, it replaces it.

#### `putBucketLogging`

``` purescript
putBucketLogging :: forall eff. Service -> PutBucketLoggingRequest -> Aff (exception :: EXCEPTION | eff) NoOutput
```

Set the logging parameters for a bucket and to specify permissions for who can view and modify the logging parameters. To set the logging status of a bucket, you must be the bucket owner.

#### `putBucketMetricsConfiguration`

``` purescript
putBucketMetricsConfiguration :: forall eff. Service -> PutBucketMetricsConfigurationRequest -> Aff (exception :: EXCEPTION | eff) NoOutput
```

Sets a metrics configuration (specified by the metrics configuration ID) for the bucket.

#### `putBucketNotification`

``` purescript
putBucketNotification :: forall eff. Service -> PutBucketNotificationRequest -> Aff (exception :: EXCEPTION | eff) NoOutput
```

Deprecated, see the PutBucketNotificationConfiguraiton operation.

#### `putBucketNotificationConfiguration`

``` purescript
putBucketNotificationConfiguration :: forall eff. Service -> PutBucketNotificationConfigurationRequest -> Aff (exception :: EXCEPTION | eff) NoOutput
```

Enables notifications of specified events for a bucket.

#### `putBucketPolicy`

``` purescript
putBucketPolicy :: forall eff. Service -> PutBucketPolicyRequest -> Aff (exception :: EXCEPTION | eff) NoOutput
```

Replaces a policy on a bucket. If the bucket already has a policy, the one in this request completely replaces it.

#### `putBucketReplication`

``` purescript
putBucketReplication :: forall eff. Service -> PutBucketReplicationRequest -> Aff (exception :: EXCEPTION | eff) NoOutput
```

Creates a new replication configuration (or replaces an existing one, if present).

#### `putBucketRequestPayment`

``` purescript
putBucketRequestPayment :: forall eff. Service -> PutBucketRequestPaymentRequest -> Aff (exception :: EXCEPTION | eff) NoOutput
```

Sets the request payment configuration for a bucket. By default, the bucket owner pays for downloads from the bucket. This configuration parameter enables the bucket owner (only) to specify that the person requesting the download will be charged for the download. Documentation on requester pays buckets can be found at http://docs.aws.amazon.com/AmazonS3/latest/dev/RequesterPaysBuckets.html

#### `putBucketTagging`

``` purescript
putBucketTagging :: forall eff. Service -> PutBucketTaggingRequest -> Aff (exception :: EXCEPTION | eff) NoOutput
```

Sets the tags for a bucket.

#### `putBucketVersioning`

``` purescript
putBucketVersioning :: forall eff. Service -> PutBucketVersioningRequest -> Aff (exception :: EXCEPTION | eff) NoOutput
```

Sets the versioning state of an existing bucket. To set the versioning state, you must be the bucket owner.

#### `putBucketWebsite`

``` purescript
putBucketWebsite :: forall eff. Service -> PutBucketWebsiteRequest -> Aff (exception :: EXCEPTION | eff) NoOutput
```

Set the website configuration for a bucket.

#### `putObject`

``` purescript
putObject :: forall eff. Service -> PutObjectRequest -> Aff (exception :: EXCEPTION | eff) PutObjectOutput
```

Adds an object to a bucket.

#### `putObjectAcl`

``` purescript
putObjectAcl :: forall eff. Service -> PutObjectAclRequest -> Aff (exception :: EXCEPTION | eff) PutObjectAclOutput
```

uses the acl subresource to set the access control list (ACL) permissions for an object that already exists in a bucket

#### `putObjectTagging`

``` purescript
putObjectTagging :: forall eff. Service -> PutObjectTaggingRequest -> Aff (exception :: EXCEPTION | eff) PutObjectTaggingOutput
```

Sets the supplied tag-set to an object that already exists in a bucket

#### `restoreObject`

``` purescript
restoreObject :: forall eff. Service -> RestoreObjectRequest -> Aff (exception :: EXCEPTION | eff) RestoreObjectOutput
```

Restores an archived copy of an object back into Amazon S3

#### `uploadPart`

``` purescript
uploadPart :: forall eff. Service -> UploadPartRequest -> Aff (exception :: EXCEPTION | eff) UploadPartOutput
```

<p>Uploads a part in a multipart upload.</p><p><b>Note:</b> After you initiate multipart upload and upload one or more parts, you must either complete or abort multipart upload in order to stop getting charged for storage of the uploaded parts. Only after you either complete or abort multipart upload, Amazon S3 frees up the parts storage and stops charging you for the parts storage.</p>

#### `uploadPartCopy`

``` purescript
uploadPartCopy :: forall eff. Service -> UploadPartCopyRequest -> Aff (exception :: EXCEPTION | eff) UploadPartCopyOutput
```

Uploads a part by copying data from an existing object as data source.


