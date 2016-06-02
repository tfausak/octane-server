'use strict';

const aws = require('aws-sdk');
const express = require('express');
const fs = require('fs');
const morgan = require('morgan');
const multer = require('multer');
const spawn = require('child_process').spawn;

// Configuration

// AWS_ACCESS_KEY_ID
const AWS_REGION = process.env.AWS_REGION || 'us-east-1';
// AWS_SECRET_ACCESS_KEY
const PORT = process.env.PORT || 8080;
const S3_BUCKET = process.env.S3_BUCKET;

// AWS

aws.config.update({ region: AWS_REGION });
const s3 = new aws.S3({ params: { Bucket: S3_BUCKET } });

// Multer

const upload = multer({ dest: './tmp' });

// API v1

const apiV1 = express.Router();

apiV1.post('/replays', upload.single('replay'), (request, response) => {
  const name = request.file.originalname;
  const path = request.file.path;

  // Upload the replay to S3 if it's not already there.
  s3.headObject({ Key: name }, (error, _data) => {
    if (!error) { return; }
    if (error.code !== 'NotFound') { return console.error(error); }
    fs.readFile(path, (error, data) => {
      if (error) { return console.error(error); }
      s3.upload({ Body: data, Key: name }, (error, _data) => {
        if (error) { return console.error(error); }
      });
    });
  });

  // Convert the replay to JSON with Octane.
  const octane = spawn('./bin/octane', [path]);
  var output = '';
  octane.stdout.on('data', (data) => {
    output += data;
  });
  octane.on('close', (status) => {
    if (status !== 0) { return response.status(500).json(null); }
    response.status(200).json(JSON.parse(output));
  });
});

// API

const api = express.Router();
api.use('/v1', apiV1);

// App

const app = express();
app.use(morgan('combined'));
app.use('/api', api);
app.use(express.static('static'));

// Server

const server = app.listen(PORT, () => {
  console.log('Listening on port ' + server.address().port + '...');
});
