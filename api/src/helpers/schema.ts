import { GraphQLObjectType, GraphQLString, GraphQLList, GraphQLSchema } from 'graphql';
import axios, { AxiosRequestConfig } from 'axios';

/**
 * Professor Query type
 */
const ProfessorType = new GraphQLObjectType({
	name: 'Professor',
	fields: () => ({
		ucinetid: { type: GraphQLString },
		name: { type: GraphQLString }
	})
})

/**
 * Course Query type
 */
const CourseType = new GraphQLObjectType({
	name: 'Course',
	fields: () => ({
		id: { type: GraphQLString },
		instructor_history: { type: new GraphQLList(ProfessorType) }
	})
})


/**
 * Configuration Details that need to be passed
 */
var config: AxiosRequestConfig = {
	method: 'get',
	url: process.env.PUBLIC_API_GRAPHQL_URL,
	headers: {
		'Accept-Encoding': 'gzip, deflate, br',
		'Content-Type': 'application/json',
		'Accept': 'application/json',
		'Connection': 'keep-alive',
		'DNT': '1',
		'x-api-key': process.env.PPAPI_KEY
	},
	data: ''
};

/**
 * The Root Query
 */
const RootQuery = new GraphQLObjectType({
	name: 'RootQueryType',
	fields: {
		course: {
			type: CourseType,
			args: {
				id: { type: GraphQLString },
			},
			resolve(parent, args) {
				var data = JSON.stringify({
					'query': `query {
						course (id: "${args.id}"){ 
							instructor_history {
								name 
								ucinetid
							}
						}
					}`
				});
				config.data = data;
				return axios(config).then((res) => res.data.data.course);
			}

		},
		allinstructors: {
			type: new GraphQLList(ProfessorType),
			resolve(parent, args) {
				var data = JSON.stringify({
					'query': `query {
						allInstructors{ 
							ucinetid  
							name
						}
					}`
				});
				config.data = data;
				return axios(config).then((res) => res.data.data.allInstructors);
			}
		},
		instructor: {
			type: ProfessorType,
			args: {
				ucinetid: { type: GraphQLString }
			},
			resolve(parent, args) {
				var data = JSON.stringify({
					'query': `query { 
						instructor(ucinetid: "${args.ucinetid}" ) { 
							name
						}
					}`
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


export default new GraphQLSchema({
	query: RootQuery
});