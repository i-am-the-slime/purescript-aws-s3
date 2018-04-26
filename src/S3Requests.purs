
module AWS.S3.Requests where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (EXCEPTION)

import AWS.Request (MethodName(..), request) as AWS
import AWS.Request.Types as Types

import AWS.S3 as S3
import AWS.S3.Types as S3Types


-- | <p>Aborts a multipart upload.</p><p>To verify that all parts have been removed, so you don't get charged for the part storage, you should call the List Parts operation and ensure the parts list is empty.</p>
abortMultipartUpload :: forall eff. S3.Service -> S3Types.AbortMultipartUploadRequest -> Aff (exception :: EXCEPTION | eff) S3Types.AbortMultipartUploadOutput
abortMultipartUpload (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "abortMultipartUpload"


-- | Completes a multipart upload by assembling previously uploaded parts.
completeMultipartUpload :: forall eff. S3.Service -> S3Types.CompleteMultipartUploadRequest -> Aff (exception :: EXCEPTION | eff) S3Types.CompleteMultipartUploadOutput
completeMultipartUpload (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "completeMultipartUpload"


-- | Creates a copy of an object that is already stored in Amazon S3.
copyObject :: forall eff. S3.Service -> S3Types.CopyObjectRequest -> Aff (exception :: EXCEPTION | eff) S3Types.CopyObjectOutput
copyObject (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "copyObject"


-- | Creates a new bucket.
createBucket :: forall eff. S3.Service -> S3Types.CreateBucketRequest -> Aff (exception :: EXCEPTION | eff) S3Types.CreateBucketOutput
createBucket (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "createBucket"


-- | <p>Initiates a multipart upload and returns an upload ID.</p><p><b>Note:</b> After you initiate multipart upload and upload one or more parts, you must either complete or abort multipart upload in order to stop getting charged for storage of the uploaded parts. Only after you either complete or abort multipart upload, Amazon S3 frees up the parts storage and stops charging you for the parts storage.</p>
createMultipartUpload :: forall eff. S3.Service -> S3Types.CreateMultipartUploadRequest -> Aff (exception :: EXCEPTION | eff) S3Types.CreateMultipartUploadOutput
createMultipartUpload (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "createMultipartUpload"


-- | Deletes the bucket. All objects (including all object versions and Delete Markers) in the bucket must be deleted before the bucket itself can be deleted.
deleteBucket :: forall eff. S3.Service -> S3Types.DeleteBucketRequest -> Aff (exception :: EXCEPTION | eff) Unit
deleteBucket (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "deleteBucket"


-- | Deletes an analytics configuration for the bucket (specified by the analytics configuration ID).
deleteBucketAnalyticsConfiguration :: forall eff. S3.Service -> S3Types.DeleteBucketAnalyticsConfigurationRequest -> Aff (exception :: EXCEPTION | eff) Unit
deleteBucketAnalyticsConfiguration (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "deleteBucketAnalyticsConfiguration"


-- | Deletes the cors configuration information set for the bucket.
deleteBucketCors :: forall eff. S3.Service -> S3Types.DeleteBucketCorsRequest -> Aff (exception :: EXCEPTION | eff) Unit
deleteBucketCors (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "deleteBucketCors"


-- | Deletes the server-side encryption configuration from the bucket.
deleteBucketEncryption :: forall eff. S3.Service -> S3Types.DeleteBucketEncryptionRequest -> Aff (exception :: EXCEPTION | eff) Unit
deleteBucketEncryption (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "deleteBucketEncryption"


-- | Deletes an inventory configuration (identified by the inventory ID) from the bucket.
deleteBucketInventoryConfiguration :: forall eff. S3.Service -> S3Types.DeleteBucketInventoryConfigurationRequest -> Aff (exception :: EXCEPTION | eff) Unit
deleteBucketInventoryConfiguration (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "deleteBucketInventoryConfiguration"


-- | Deletes the lifecycle configuration from the bucket.
deleteBucketLifecycle :: forall eff. S3.Service -> S3Types.DeleteBucketLifecycleRequest -> Aff (exception :: EXCEPTION | eff) Unit
deleteBucketLifecycle (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "deleteBucketLifecycle"


-- | Deletes a metrics configuration (specified by the metrics configuration ID) from the bucket.
deleteBucketMetricsConfiguration :: forall eff. S3.Service -> S3Types.DeleteBucketMetricsConfigurationRequest -> Aff (exception :: EXCEPTION | eff) Unit
deleteBucketMetricsConfiguration (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "deleteBucketMetricsConfiguration"


-- | Deletes the policy from the bucket.
deleteBucketPolicy :: forall eff. S3.Service -> S3Types.DeleteBucketPolicyRequest -> Aff (exception :: EXCEPTION | eff) Unit
deleteBucketPolicy (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "deleteBucketPolicy"


-- | Deletes the replication configuration from the bucket.
deleteBucketReplication :: forall eff. S3.Service -> S3Types.DeleteBucketReplicationRequest -> Aff (exception :: EXCEPTION | eff) Unit
deleteBucketReplication (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "deleteBucketReplication"


-- | Deletes the tags from the bucket.
deleteBucketTagging :: forall eff. S3.Service -> S3Types.DeleteBucketTaggingRequest -> Aff (exception :: EXCEPTION | eff) Unit
deleteBucketTagging (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "deleteBucketTagging"


-- | This operation removes the website configuration from the bucket.
deleteBucketWebsite :: forall eff. S3.Service -> S3Types.DeleteBucketWebsiteRequest -> Aff (exception :: EXCEPTION | eff) Unit
deleteBucketWebsite (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "deleteBucketWebsite"


-- | Removes the null version (if there is one) of an object and inserts a delete marker, which becomes the latest version of the object. If there isn't a null version, Amazon S3 does not remove any objects.
deleteObject :: forall eff. S3.Service -> S3Types.DeleteObjectRequest -> Aff (exception :: EXCEPTION | eff) S3Types.DeleteObjectOutput
deleteObject (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "deleteObject"


-- | Removes the tag-set from an existing object.
deleteObjectTagging :: forall eff. S3.Service -> S3Types.DeleteObjectTaggingRequest -> Aff (exception :: EXCEPTION | eff) S3Types.DeleteObjectTaggingOutput
deleteObjectTagging (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "deleteObjectTagging"


-- | This operation enables you to delete multiple objects from a bucket using a single HTTP request. You may specify up to 1000 keys.
deleteObjects :: forall eff. S3.Service -> S3Types.DeleteObjectsRequest -> Aff (exception :: EXCEPTION | eff) S3Types.DeleteObjectsOutput
deleteObjects (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "deleteObjects"


-- | Returns the accelerate configuration of a bucket.
getBucketAccelerateConfiguration :: forall eff. S3.Service -> S3Types.GetBucketAccelerateConfigurationRequest -> Aff (exception :: EXCEPTION | eff) S3Types.GetBucketAccelerateConfigurationOutput
getBucketAccelerateConfiguration (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "getBucketAccelerateConfiguration"


-- | Gets the access control policy for the bucket.
getBucketAcl :: forall eff. S3.Service -> S3Types.GetBucketAclRequest -> Aff (exception :: EXCEPTION | eff) S3Types.GetBucketAclOutput
getBucketAcl (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "getBucketAcl"


-- | Gets an analytics configuration for the bucket (specified by the analytics configuration ID).
getBucketAnalyticsConfiguration :: forall eff. S3.Service -> S3Types.GetBucketAnalyticsConfigurationRequest -> Aff (exception :: EXCEPTION | eff) S3Types.GetBucketAnalyticsConfigurationOutput
getBucketAnalyticsConfiguration (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "getBucketAnalyticsConfiguration"


-- | Returns the cors configuration for the bucket.
getBucketCors :: forall eff. S3.Service -> S3Types.GetBucketCorsRequest -> Aff (exception :: EXCEPTION | eff) S3Types.GetBucketCorsOutput
getBucketCors (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "getBucketCors"


-- | Returns the server-side encryption configuration of a bucket.
getBucketEncryption :: forall eff. S3.Service -> S3Types.GetBucketEncryptionRequest -> Aff (exception :: EXCEPTION | eff) S3Types.GetBucketEncryptionOutput
getBucketEncryption (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "getBucketEncryption"


-- | Returns an inventory configuration (identified by the inventory ID) from the bucket.
getBucketInventoryConfiguration :: forall eff. S3.Service -> S3Types.GetBucketInventoryConfigurationRequest -> Aff (exception :: EXCEPTION | eff) S3Types.GetBucketInventoryConfigurationOutput
getBucketInventoryConfiguration (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "getBucketInventoryConfiguration"


-- | Deprecated, see the GetBucketLifecycleConfiguration operation.
getBucketLifecycle :: forall eff. S3.Service -> S3Types.GetBucketLifecycleRequest -> Aff (exception :: EXCEPTION | eff) S3Types.GetBucketLifecycleOutput
getBucketLifecycle (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "getBucketLifecycle"


-- | Returns the lifecycle configuration information set on the bucket.
getBucketLifecycleConfiguration :: forall eff. S3.Service -> S3Types.GetBucketLifecycleConfigurationRequest -> Aff (exception :: EXCEPTION | eff) S3Types.GetBucketLifecycleConfigurationOutput
getBucketLifecycleConfiguration (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "getBucketLifecycleConfiguration"


-- | Returns the region the bucket resides in.
getBucketLocation :: forall eff. S3.Service -> S3Types.GetBucketLocationRequest -> Aff (exception :: EXCEPTION | eff) S3Types.GetBucketLocationOutput
getBucketLocation (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "getBucketLocation"


-- | Returns the logging status of a bucket and the permissions users have to view and modify that status. To use GET, you must be the bucket owner.
getBucketLogging :: forall eff. S3.Service -> S3Types.GetBucketLoggingRequest -> Aff (exception :: EXCEPTION | eff) S3Types.GetBucketLoggingOutput
getBucketLogging (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "getBucketLogging"


-- | Gets a metrics configuration (specified by the metrics configuration ID) from the bucket.
getBucketMetricsConfiguration :: forall eff. S3.Service -> S3Types.GetBucketMetricsConfigurationRequest -> Aff (exception :: EXCEPTION | eff) S3Types.GetBucketMetricsConfigurationOutput
getBucketMetricsConfiguration (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "getBucketMetricsConfiguration"


-- | Deprecated, see the GetBucketNotificationConfiguration operation.
getBucketNotification :: forall eff. S3.Service -> S3Types.GetBucketNotificationConfigurationRequest -> Aff (exception :: EXCEPTION | eff) S3Types.NotificationConfigurationDeprecated
getBucketNotification (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "getBucketNotification"


-- | Returns the notification configuration of a bucket.
getBucketNotificationConfiguration :: forall eff. S3.Service -> S3Types.GetBucketNotificationConfigurationRequest -> Aff (exception :: EXCEPTION | eff) S3Types.NotificationConfiguration
getBucketNotificationConfiguration (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "getBucketNotificationConfiguration"


-- | Returns the policy of a specified bucket.
getBucketPolicy :: forall eff. S3.Service -> S3Types.GetBucketPolicyRequest -> Aff (exception :: EXCEPTION | eff) S3Types.GetBucketPolicyOutput
getBucketPolicy (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "getBucketPolicy"


-- | Returns the replication configuration of a bucket.
getBucketReplication :: forall eff. S3.Service -> S3Types.GetBucketReplicationRequest -> Aff (exception :: EXCEPTION | eff) S3Types.GetBucketReplicationOutput
getBucketReplication (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "getBucketReplication"


-- | Returns the request payment configuration of a bucket.
getBucketRequestPayment :: forall eff. S3.Service -> S3Types.GetBucketRequestPaymentRequest -> Aff (exception :: EXCEPTION | eff) S3Types.GetBucketRequestPaymentOutput
getBucketRequestPayment (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "getBucketRequestPayment"


-- | Returns the tag set associated with the bucket.
getBucketTagging :: forall eff. S3.Service -> S3Types.GetBucketTaggingRequest -> Aff (exception :: EXCEPTION | eff) S3Types.GetBucketTaggingOutput
getBucketTagging (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "getBucketTagging"


-- | Returns the versioning state of a bucket.
getBucketVersioning :: forall eff. S3.Service -> S3Types.GetBucketVersioningRequest -> Aff (exception :: EXCEPTION | eff) S3Types.GetBucketVersioningOutput
getBucketVersioning (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "getBucketVersioning"


-- | Returns the website configuration for a bucket.
getBucketWebsite :: forall eff. S3.Service -> S3Types.GetBucketWebsiteRequest -> Aff (exception :: EXCEPTION | eff) S3Types.GetBucketWebsiteOutput
getBucketWebsite (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "getBucketWebsite"


-- | Retrieves objects from Amazon S3.
getObject :: forall eff. S3.Service -> S3Types.GetObjectRequest -> Aff (exception :: EXCEPTION | eff) S3Types.GetObjectOutput
getObject (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "getObject"


-- | Returns the access control list (ACL) of an object.
getObjectAcl :: forall eff. S3.Service -> S3Types.GetObjectAclRequest -> Aff (exception :: EXCEPTION | eff) S3Types.GetObjectAclOutput
getObjectAcl (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "getObjectAcl"


-- | Returns the tag-set of an object.
getObjectTagging :: forall eff. S3.Service -> S3Types.GetObjectTaggingRequest -> Aff (exception :: EXCEPTION | eff) S3Types.GetObjectTaggingOutput
getObjectTagging (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "getObjectTagging"


-- | Return torrent files from a bucket.
getObjectTorrent :: forall eff. S3.Service -> S3Types.GetObjectTorrentRequest -> Aff (exception :: EXCEPTION | eff) S3Types.GetObjectTorrentOutput
getObjectTorrent (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "getObjectTorrent"


-- | This operation is useful to determine if a bucket exists and you have permission to access it.
headBucket :: forall eff. S3.Service -> S3Types.HeadBucketRequest -> Aff (exception :: EXCEPTION | eff) Unit
headBucket (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "headBucket"


-- | The HEAD operation retrieves metadata from an object without returning the object itself. This operation is useful if you're only interested in an object's metadata. To use HEAD, you must have READ access to the object.
headObject :: forall eff. S3.Service -> S3Types.HeadObjectRequest -> Aff (exception :: EXCEPTION | eff) S3Types.HeadObjectOutput
headObject (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "headObject"


-- | Lists the analytics configurations for the bucket.
listBucketAnalyticsConfigurations :: forall eff. S3.Service -> S3Types.ListBucketAnalyticsConfigurationsRequest -> Aff (exception :: EXCEPTION | eff) S3Types.ListBucketAnalyticsConfigurationsOutput
listBucketAnalyticsConfigurations (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "listBucketAnalyticsConfigurations"


-- | Returns a list of inventory configurations for the bucket.
listBucketInventoryConfigurations :: forall eff. S3.Service -> S3Types.ListBucketInventoryConfigurationsRequest -> Aff (exception :: EXCEPTION | eff) S3Types.ListBucketInventoryConfigurationsOutput
listBucketInventoryConfigurations (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "listBucketInventoryConfigurations"


-- | Lists the metrics configurations for the bucket.
listBucketMetricsConfigurations :: forall eff. S3.Service -> S3Types.ListBucketMetricsConfigurationsRequest -> Aff (exception :: EXCEPTION | eff) S3Types.ListBucketMetricsConfigurationsOutput
listBucketMetricsConfigurations (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "listBucketMetricsConfigurations"


-- | Returns a list of all buckets owned by the authenticated sender of the request.
listBuckets :: forall eff. S3.Service ->  Aff (exception :: EXCEPTION | eff) S3Types.ListBucketsOutput
listBuckets (S3.Service serviceImpl) = AWS.request serviceImpl method unit where
    method = AWS.MethodName "listBuckets"


-- | This operation lists in-progress multipart uploads.
listMultipartUploads :: forall eff. S3.Service -> S3Types.ListMultipartUploadsRequest -> Aff (exception :: EXCEPTION | eff) S3Types.ListMultipartUploadsOutput
listMultipartUploads (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "listMultipartUploads"


-- | Returns metadata about all of the versions of objects in a bucket.
listObjectVersions :: forall eff. S3.Service -> S3Types.ListObjectVersionsRequest -> Aff (exception :: EXCEPTION | eff) S3Types.ListObjectVersionsOutput
listObjectVersions (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "listObjectVersions"


-- | Returns some or all (up to 1000) of the objects in a bucket. You can use the request parameters as selection criteria to return a subset of the objects in a bucket.
listObjects :: forall eff. S3.Service -> S3Types.ListObjectsRequest -> Aff (exception :: EXCEPTION | eff) S3Types.ListObjectsOutput
listObjects (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "listObjects"


-- | Returns some or all (up to 1000) of the objects in a bucket. You can use the request parameters as selection criteria to return a subset of the objects in a bucket. Note: ListObjectsV2 is the revised List Objects API and we recommend you use this revised API for new application development.
listObjectsV2 :: forall eff. S3.Service -> S3Types.ListObjectsV2Request -> Aff (exception :: EXCEPTION | eff) S3Types.ListObjectsV2Output
listObjectsV2 (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "listObjectsV2"


-- | Lists the parts that have been uploaded for a specific multipart upload.
listParts :: forall eff. S3.Service -> S3Types.ListPartsRequest -> Aff (exception :: EXCEPTION | eff) S3Types.ListPartsOutput
listParts (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "listParts"


-- | Sets the accelerate configuration of an existing bucket.
putBucketAccelerateConfiguration :: forall eff. S3.Service -> S3Types.PutBucketAccelerateConfigurationRequest -> Aff (exception :: EXCEPTION | eff) Unit
putBucketAccelerateConfiguration (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "putBucketAccelerateConfiguration"


-- | Sets the permissions on a bucket using access control lists (ACL).
putBucketAcl :: forall eff. S3.Service -> S3Types.PutBucketAclRequest -> Aff (exception :: EXCEPTION | eff) Unit
putBucketAcl (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "putBucketAcl"


-- | Sets an analytics configuration for the bucket (specified by the analytics configuration ID).
putBucketAnalyticsConfiguration :: forall eff. S3.Service -> S3Types.PutBucketAnalyticsConfigurationRequest -> Aff (exception :: EXCEPTION | eff) Unit
putBucketAnalyticsConfiguration (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "putBucketAnalyticsConfiguration"


-- | Sets the cors configuration for a bucket.
putBucketCors :: forall eff. S3.Service -> S3Types.PutBucketCorsRequest -> Aff (exception :: EXCEPTION | eff) Unit
putBucketCors (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "putBucketCors"


-- | Creates a new server-side encryption configuration (or replaces an existing one, if present).
putBucketEncryption :: forall eff. S3.Service -> S3Types.PutBucketEncryptionRequest -> Aff (exception :: EXCEPTION | eff) Unit
putBucketEncryption (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "putBucketEncryption"


-- | Adds an inventory configuration (identified by the inventory ID) from the bucket.
putBucketInventoryConfiguration :: forall eff. S3.Service -> S3Types.PutBucketInventoryConfigurationRequest -> Aff (exception :: EXCEPTION | eff) Unit
putBucketInventoryConfiguration (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "putBucketInventoryConfiguration"


-- | Deprecated, see the PutBucketLifecycleConfiguration operation.
putBucketLifecycle :: forall eff. S3.Service -> S3Types.PutBucketLifecycleRequest -> Aff (exception :: EXCEPTION | eff) Unit
putBucketLifecycle (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "putBucketLifecycle"


-- | Sets lifecycle configuration for your bucket. If a lifecycle configuration exists, it replaces it.
putBucketLifecycleConfiguration :: forall eff. S3.Service -> S3Types.PutBucketLifecycleConfigurationRequest -> Aff (exception :: EXCEPTION | eff) Unit
putBucketLifecycleConfiguration (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "putBucketLifecycleConfiguration"


-- | Set the logging parameters for a bucket and to specify permissions for who can view and modify the logging parameters. To set the logging status of a bucket, you must be the bucket owner.
putBucketLogging :: forall eff. S3.Service -> S3Types.PutBucketLoggingRequest -> Aff (exception :: EXCEPTION | eff) Unit
putBucketLogging (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "putBucketLogging"


-- | Sets a metrics configuration (specified by the metrics configuration ID) for the bucket.
putBucketMetricsConfiguration :: forall eff. S3.Service -> S3Types.PutBucketMetricsConfigurationRequest -> Aff (exception :: EXCEPTION | eff) Unit
putBucketMetricsConfiguration (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "putBucketMetricsConfiguration"


-- | Deprecated, see the PutBucketNotificationConfiguraiton operation.
putBucketNotification :: forall eff. S3.Service -> S3Types.PutBucketNotificationRequest -> Aff (exception :: EXCEPTION | eff) Unit
putBucketNotification (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "putBucketNotification"


-- | Enables notifications of specified events for a bucket.
putBucketNotificationConfiguration :: forall eff. S3.Service -> S3Types.PutBucketNotificationConfigurationRequest -> Aff (exception :: EXCEPTION | eff) Unit
putBucketNotificationConfiguration (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "putBucketNotificationConfiguration"


-- | Replaces a policy on a bucket. If the bucket already has a policy, the one in this request completely replaces it.
putBucketPolicy :: forall eff. S3.Service -> S3Types.PutBucketPolicyRequest -> Aff (exception :: EXCEPTION | eff) Unit
putBucketPolicy (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "putBucketPolicy"


-- | Creates a new replication configuration (or replaces an existing one, if present).
putBucketReplication :: forall eff. S3.Service -> S3Types.PutBucketReplicationRequest -> Aff (exception :: EXCEPTION | eff) Unit
putBucketReplication (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "putBucketReplication"


-- | Sets the request payment configuration for a bucket. By default, the bucket owner pays for downloads from the bucket. This configuration parameter enables the bucket owner (only) to specify that the person requesting the download will be charged for the download. Documentation on requester pays buckets can be found at http://docs.aws.amazon.com/AmazonS3/latest/dev/RequesterPaysBuckets.html
putBucketRequestPayment :: forall eff. S3.Service -> S3Types.PutBucketRequestPaymentRequest -> Aff (exception :: EXCEPTION | eff) Unit
putBucketRequestPayment (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "putBucketRequestPayment"


-- | Sets the tags for a bucket.
putBucketTagging :: forall eff. S3.Service -> S3Types.PutBucketTaggingRequest -> Aff (exception :: EXCEPTION | eff) Unit
putBucketTagging (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "putBucketTagging"


-- | Sets the versioning state of an existing bucket. To set the versioning state, you must be the bucket owner.
putBucketVersioning :: forall eff. S3.Service -> S3Types.PutBucketVersioningRequest -> Aff (exception :: EXCEPTION | eff) Unit
putBucketVersioning (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "putBucketVersioning"


-- | Set the website configuration for a bucket.
putBucketWebsite :: forall eff. S3.Service -> S3Types.PutBucketWebsiteRequest -> Aff (exception :: EXCEPTION | eff) Unit
putBucketWebsite (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "putBucketWebsite"


-- | Adds an object to a bucket.
putObject :: forall eff. S3.Service -> S3Types.PutObjectRequest -> Aff (exception :: EXCEPTION | eff) S3Types.PutObjectOutput
putObject (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "putObject"


-- | uses the acl subresource to set the access control list (ACL) permissions for an object that already exists in a bucket
putObjectAcl :: forall eff. S3.Service -> S3Types.PutObjectAclRequest -> Aff (exception :: EXCEPTION | eff) S3Types.PutObjectAclOutput
putObjectAcl (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "putObjectAcl"


-- | Sets the supplied tag-set to an object that already exists in a bucket
putObjectTagging :: forall eff. S3.Service -> S3Types.PutObjectTaggingRequest -> Aff (exception :: EXCEPTION | eff) S3Types.PutObjectTaggingOutput
putObjectTagging (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "putObjectTagging"


-- | Restores an archived copy of an object back into Amazon S3
restoreObject :: forall eff. S3.Service -> S3Types.RestoreObjectRequest -> Aff (exception :: EXCEPTION | eff) S3Types.RestoreObjectOutput
restoreObject (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "restoreObject"


-- | <p>Uploads a part in a multipart upload.</p><p><b>Note:</b> After you initiate multipart upload and upload one or more parts, you must either complete or abort multipart upload in order to stop getting charged for storage of the uploaded parts. Only after you either complete or abort multipart upload, Amazon S3 frees up the parts storage and stops charging you for the parts storage.</p>
uploadPart :: forall eff. S3.Service -> S3Types.UploadPartRequest -> Aff (exception :: EXCEPTION | eff) S3Types.UploadPartOutput
uploadPart (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "uploadPart"


-- | Uploads a part by copying data from an existing object as data source.
uploadPartCopy :: forall eff. S3.Service -> S3Types.UploadPartCopyRequest -> Aff (exception :: EXCEPTION | eff) S3Types.UploadPartCopyOutput
uploadPartCopy (S3.Service serviceImpl) = AWS.request serviceImpl method  where
    method = AWS.MethodName "uploadPartCopy"
