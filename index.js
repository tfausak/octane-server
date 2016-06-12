'use strict';

const aws = require('aws-sdk');
const child = require('child_process');
const express = require('express');
const fs = require('fs');
const knex = require('knex');
const morgan = require('morgan');
const multer = require('multer');
const uuid = require('uuid');

// Configuration

// AWS_ACCESS_KEY_ID
const AWS_REGION = process.env.AWS_REGION || 'us-east-1';
// AWS_SECRET_ACCESS_KEY
// DATABASE_URL
const HOST = process.env.HOST || '127.0.0.1';
const PORT = process.env.PORT || 8080;
const S3_BUCKET = process.env.S3_BUCKET;

// AWS

aws.config.update({ region: AWS_REGION });
const s3 = new aws.S3({ params: { Bucket: S3_BUCKET } });

// Database

const db = knex(require('./knexfile'));

// Multer

const upload = multer({ dest: './tmp' });

// API v1

const apiV1 = express.Router();

apiV1.post('/replays', upload.single('replay'), (request, response) => {
  response.set('Content-Type', 'application/json');

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
  const octane = child.spawn(`./bin/octane-0.9.0-${process.platform}`, [path]);
  var stderr = '';
  octane.stdout.on('data', (data) => { response.write(data.toString()); });
  octane.stderr.on('data', (data) => { stderr += data.toString(); });
  octane.on('close', (status) => {
    if (status !== 0) {
      console.error(stderr);
      return response.status(500).end();
    }
    response.status(200).end();
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

const server = app.listen(PORT, HOST, () => {
  console.log(`Listening on port ${PORT} of ${HOST}...`);
});
