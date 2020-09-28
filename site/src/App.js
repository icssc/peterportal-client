import React, { Component } from 'react'
import {
  BrowserRouter as Router,
  Switch, Redirect,
  Route,
} from 'react-router-dom'
import NavBar from './component/NavBar.jsx'
import Footer from './component/Footer.jsx'
import SearchPage from "./pages/SearchPage/SearchPage.jsx";
import CoursePage from "./pages/CoursePage/CoursePage.jsx";
// import { getSession } from './utils'

export default class App extends Component {

  constructor(props) {
    super(props)
    this.state = {}
  }

  async componentDidMount() {
    // console.log(getSession())
  }

  render() {
    return (
      <Router>
        <NavBar />
        <Switch>
          <Route exact path="/">
            <Redirect to="/search/courses" />
          </Route>

          <Route path="/search/:index" component={SearchPage} />
          <Route path="/course/:id" component={CoursePage} />


          {/* <Route path='/register'>
            <Auth />
          </Route>

          <Route path='/login'>
            <Auth />
          </Route>

          <PrivateRoute
            exact
            path='/'
            component={Dashboard}
          /> */}

        </Switch>

        <Footer />
      </Router>


    )
  }
}

/**
 * A component to protect routes.
 * Shows Auth page if the user is not authenticated
 */
// const PrivateRoute = ({ component, ...options }) => {

//   const session = getSession()

//   const finalComponent = session ? Dashboard : Home
//   return <Route {...options} component={finalComponent} />
// }