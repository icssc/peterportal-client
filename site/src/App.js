import React from 'react'; 

import {
  BrowserRouter as Router,
  Switch, Redirect,
  Route
} from 'react-router-dom';
import 'bootstrap/dist/css/bootstrap.min.css';

import AppHeader from './component/AppHeader/AppHeader.jsx';
import Footer from './component/Footer/Footer';

import SearchPage from "./pages/SearchPage";
import CoursePage from "./pages/CoursePage/CoursePage.jsx";
import ErrorPage from "./pages/ErrorPage";
import RoadmapPage from "./pages/RoadmapPage";
import ProfessorPage from "./pages/ProfessorPage";
import ZotisticsPage from "./pages/ZotisticsPage";
import Schedule from "./component/Schedule/Schedule";
import Sidebar from "./component/SideBar/SideBar";

//Create a Client
import { ApolloClient, InMemoryCache } from '@apollo/client';
import { gql } from '@apollo/client';
import { ApolloProvider } from '@apollo/client/react';

const client = new ApolloClient({
    uri: 'http://localhost:5000/graphql/',
    cache: new InMemoryCache()
  });

  client
  .query({
    query: gql`
      query GetIntstructor {
        instructor(ucinetid: "kakagi") {
          name
        }
      }
    `
  })
  .then(result => console.log(result.data));


export default function App() {
  return (
    <ApolloProvider client={client}>
    <Router>
    <AppHeader/>  
        <div style={{display: "flex", padding: "4rem 3rem"}}>
        <Switch>
          <Route exact path="/">
            <Redirect to="/search/courses" />
          </Route>
          <Route path="/search/:index" />
          <Route component={Sidebar} />
        </Switch>
          <Switch>
            <Route exact path="/">
              <Redirect to="/search/courses" />
            </Route>
            <Route path="/roadmap" component={RoadmapPage}/>
            <Route path="/zotistics" component={ZotisticsPage} />
            <Route path="/search/:index" component={SearchPage} />
            <Route path="/course/:id" component={CoursePage} />
            <Route path="/professor/:id" component={ProfessorPage} />
            <Route path="/schedule" component = {Schedule}></Route>
            <Route component={ErrorPage} />
          </Switch>
        </div>
      <Footer/>
    </Router>
    </ApolloProvider>

  )
}
