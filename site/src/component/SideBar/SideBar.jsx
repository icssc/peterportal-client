import React from 'react';
import { NavLink } from 'react-router-dom';
import { Icon } from 'semantic-ui-react';
import './Sidebar.scss'

const SideBar = ({children}) => {
  return (
    <div className='sidebar'>
      <ul>
        <li><NavLink to='/search/courses' activeClassName='sidebar-active'>
          <div> 
            <Icon name='list alternate outline' />
          </div>
          Catalogue
        </NavLink></li>
        {/* <li><NavLink to='/schedule' activeClassName='sidebar-active'>
          <div> 
            <Icon name='clock outline' />
          </div>
          Schedule of Classes
        </NavLink></li> */}
        {/* <li><NavLink to='/zotistics' activeClassName='sidebar-active'>
          <div> 
            <Icon name='chart bar outline' />
          </div>
          Zotistics
        </NavLink></li> */}
        {/* <li><NavLink to='/antalmanac' activeClassName='sidebar-active'>
          <div> 
            <Icon name='calendar outline' />
          </div>
          AntAlmanac
        </NavLink></li> */}
        <li><NavLink to='/roadmap' activeClassName='sidebar-active'>
          <div> 
            <Icon name='map outline' />
          </div>
          Peter's Roadmap
        </NavLink></li>
        {/* <li><NavLink to='/reviews' activeClassName='sidebar-active'>
          <div> 
            <Icon name='thumbs up outline'/>
          </div>
          Reviews
        </NavLink></li> */}
      </ul>
      {children}
    </div>
  )
}

export default SideBar
