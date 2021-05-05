
const {GraphQLObjectType, GraphOLInt, GraphQLBoolean, GraphQLString, GraphQLInt, GraphQLList, GraphQLSchema, GraphQLScalarType} = require('graphql');
const { resolve } = require('path');
const axios = require('axios');
const request = require('request');
const rp = require('request-promise');

//Launch type
// This is the Professor Type
//More types can be added like: C
const ProfessorType = new GraphQLObjectType({
	//Sakshi: add more fields
	name: 'Professor',
	fields: () => ({
		ucinetid: {type: GraphQLString},
		name: {type: GraphQLString}
	})
})

//This is the Object for Course Type
const CourseType = new GraphQLObjectType({
	//Sakshi: add more fields
	name: 'Course',
	fields: () => ({
		id: {type: GraphQLString},
		instructor_history: {type: new GraphQLList(ProfessorType)}
	})
})


//Configuration Details that need to be passed
var config = {
  method: 'get',
  url: 'https://api.peterportal.org/graphql/',
  headers: { 
    'Accept-Encoding': 'gzip, deflate, br', 
    'Content-Type': 'application/json', 
    'Accept': 'application/json', 
    'Connection': 'keep-alive', 
    'DNT': '1', 
    'Origin': 'https://api.peterportal.org', 
    'x-api-key': 'PeterPortal-293094452aaaaefd3c5ebb7dd1ad4874fb3b4d20e79be6422b9e09611f01589a'
  },
  data: ""
};


//The Root Query
const RootQuery = new GraphQLObjectType({
	name: 'RootQueryType',
	fields: {
		course: {
			type: CourseType,
			args:{
				id: {type: GraphQLString},
			},
				resolve(parent,args){
					var data = JSON.stringify({
						"query": `query{ course(id: "${args.id}" ){ instructor_history {name}}}`
					});
					config.data = data;
					return axios(config).then((res) => res.data.data.course);
				}

		},
		allinstructors: {
			type: new GraphQLList(ProfessorType),
			resolve(parent,args) {
				var data = JSON.stringify({
					"query": "query{allInstructors{ ucinetid  name}}"
				  });
				config.data = data;
				return axios(config).then((res) => res.data.data.allInstructors);
			}
		},
		instructor: {
			type: ProfessorType,
			args:{
				ucinetid: {type: GraphQLString}
			},
				resolve(parent,args) {
					var data = JSON.stringify({
						"query": `query{ instructor(ucinetid: "${args.ucinetid}" ){ name}}`
					  });
					config.data = data;
					return axios(config).then((response) => response.data.data.instructor)
					.catch(function (error) {
					  console.log(error);
					});


				}
			
		}
	}
})


module.exports = new GraphQLSchema({
	query: RootQuery
});