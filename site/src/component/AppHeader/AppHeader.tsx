import React, { useState, useEffect, Component, FC } from 'react';
import { Icon, Popup, Grid, Label, Header } from 'semantic-ui-react';
// import {useCookies} from 'react-cookie';

import { ReactComponent as CogIcon } from '../../asset/cog.svg';
import { ReactComponent as ArrowIcon } from '../../asset/arrow.svg';
import { CSSTransition } from 'react-transition-group';
import { PassportData, WeekData } from '../../types/types';

import Logo from '../../asset/peterportal-banner-logo.svg';
import './AppHeader.scss';

const AppHeader: FC<{}> = props => {
  const [week, setWeek] = useState('');
  const [name, setName] = useState('');
  const [picture, setPicture] = useState('');

  // const [cookies, setCookie] = useCookies(['name']);
  fetch('/users/getName')
    .then(res => res.json())
    .then((data: PassportData) => {
      setName(data.name);
      setPicture(data.picture);
    });

  useEffect(() => {
    fetch('/schedule/api/currentWeek')
      .then(res => res.json())
      .then((data: WeekData) => setWeek(data.display));
  })

  return (
    <header className='navbar'>
      <div className='navbar-nav'>
        <div
          style={{
            display: 'flex',
            flexDirection: 'row',
            marginRight: 'auto',
            alignItems: 'center',
          }}
        >
          <div className='peterportal-logo-container'>
            <a href='/'>
              <img alt='PeterPortal' id='peterportal-logo' src={Logo}></img>
            </a>
          </div>
          <div style={{ display: 'flex' }}>
            <div className={'school-term_container'}>
              <p className={'school-term'} style={{ marginBottom: '-1px' }}>
                {week}
              </p>
            </div>
          </div>
        </div>

        <div style={{ margin: 'auto 12px' }}>
          <Popup style={{ padding: '36px', width: '400px' }} position='bottom right' trigger={<Label as='a' color='yellow' image>alpha<Label.Detail>v0</Label.Detail></Label>} flowing hoverable >
            <Grid centered columns={1}>
              <Grid.Column textAlign='left'>
                <Header as='h4'>Alpha Disclaimer</Header>
                <p>
                  Please note that this is an alpha version of PeterPortal, which is still undergoing development.
                  Some content on this web application may not be accurate. Users are encouraged to double check details.
                  <br />
                  <br />
                  Should you encounter any bugs, glitches, lack of functionality or other problems on the application,
                  please let us know immediately so we can rectify these accordingly. Your help in this regard is greatly appreciated.
                </p>
                <a className='ui button' href='https://github.com/icssc-projects/PeterPortal/issues/new'><Icon name='github' />Report an issue</a>
              </Grid.Column>

            </Grid>
          </Popup>
        </div>
        <NavItem userPicture={picture} icon={<Icon name='user outline' />}>
          <DropdownMenu name={name} picture={picture} />
        </NavItem>
      </div>
    </header>
  );
}

interface NavItemProps {
  userPicture: string;
  icon: JSX.Element;
}

const NavItem: FC<NavItemProps> = ({ userPicture, icon, children }) => {
  const [open, setOpen] = useState(false);

  return (
    <li className='nav-item'>
      <div className='icon-button'
        style={{ backgroundImage: 'url(' + userPicture + ')', backgroundSize: 'contain' }}
        onClick={() => setOpen(!open)}>
        {!userPicture ? icon : ''}
      </div>

      {open && children}
    </li>
  );
}

interface DropdownMenuProps {
  name: string,
  picture: string,
}

const DropdownMenu: FC<DropdownMenuProps> = ({ name, picture }) => {
  const [activeMenu, setActiveMenu] = useState('main');

  interface DropdownItemProps {
    goToMenu?: string,
    className?: string,
    picture?: string,
    leftIcon?: JSX.Element,
  }

  const DropdownItem: FC<DropdownItemProps> = (props) => {
    return (
      <div
        className={['menu-item', props.className].join(' ')}
        onClick={() => props.goToMenu && setActiveMenu(props.goToMenu)}
      >
        <span className='icon-button'
          style={{ backgroundImage: 'url(' + props.picture + ')', backgroundSize: 'contain' }}>
          {props.leftIcon}
        </span>
        <span className='button-text'>{props.children}</span>
      </div>
    );
  }

  return (
    <div className='settings-menu'>
      <CSSTransition
        in={activeMenu === 'main'}
        unmountOnExit
        timeout={500}
        classNames='menu-primary'
      >
        <div className='menu'>
          {!name ?
            <DropdownItem leftIcon={<Icon name='sign in' />} goToMenu='login'>
              Log In
            </DropdownItem> : <>

              <DropdownItem picture={picture} >
                {name}
              </DropdownItem>

              <a href='/users/logout'>
                <DropdownItem leftIcon={<Icon name='log out' />}>
                  Log Out
                </DropdownItem>
              </a> </>
          }
          <DropdownItem leftIcon={<CogIcon />} goToMenu='settings'>
            Settings
          </DropdownItem>
        </div>
      </CSSTransition>

      <CSSTransition
        in={activeMenu === 'login'}
        unmountOnExit
        timeout={500}
        classNames='menu-secondary'
      >
        <div className='menu'>
          <DropdownItem leftIcon={<ArrowIcon />} goToMenu='main'></DropdownItem>
          <a href='/users/auth/google'>
            <DropdownItem
              className='google-login'
              leftIcon={<Icon name='google' />}
            >
              Log In using Google
            </DropdownItem>
          </a>
          {/* <a href='/users/auth/facebook'> 
            <DropdownItem
              className='facebook-login'
              leftIcon={<Icon name='facebook f' />}
            >
              Log In using Facebook
            </DropdownItem>
          </a> */}
          <p style={{ color: 'gray', textAlign: 'center', padding: '0.5rem 1rem' }}>By logging in, you agree to our <a href='/legal'>privacy policy</a>.</p>
        </div>
      </CSSTransition>

      <CSSTransition
        in={activeMenu === 'settings'}
        unmountOnExit
        timeout={500}
        classNames='menu-secondary'
      >
        <div className='menu'>
          <DropdownItem leftIcon={<ArrowIcon />} goToMenu='main'></DropdownItem>
          <DropdownItem
            className='dark-mode'
            leftIcon={<Icon name='moon' />}
          >
            Dark Mode <i style={{ color: 'gray' }}>Coming Soon!</i>
          </DropdownItem>
        </div>
      </CSSTransition>
    </div>
  );
}

export default AppHeader;