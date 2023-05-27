import { createGlobalStyle } from 'styled-components';
import Theme from './theme';

const GlobalStyle = createGlobalStyle<{ theme: Theme }>`
  body,
  .search-bar,
  .search-bar:focus,
  .search-bar::selection,
  .sidebar .sidebar-links ul li a,
  .side-info-feature-name a,
  .subreview-identifier a,
  .table {
    color: ${({ theme }) => theme.textColor};
  }


  .navbar-toggle-item a,
  .quarter-title,
  #year-title {
    color: ${({ theme }) => theme.quarterTitleColor}
  }

  body,
  .app-content {
    background-color: ${({ theme }) => theme.backgroundColor};
  }

  .course,
  .course-page-section,
  .hit-item,
  .modal-content,
  .navbar,
  .professor-page-section,
  .search-bar,
  .search-bar:focus,
  .search-popup,
  .search-popup-more,
  .sidebar,
  .sidebar-wrapper,
  .side-info,
  .side-info-feature-stat,
  .subreview-detail,
  .quarter,
  .year {
    background-color: ${({ theme }) => theme.overlay1Color};
  }

  .navbar {
    box-shadow: ${({ theme }) => theme.navbarBoxShadow}
  }

  .input-group-text {
    color: ${({ theme }) => theme.inputGroupText};
    background-color: ${({ theme }) => theme.overlay2Color};
  }

  .navbar-toggle,
  .prereq-text-box,
  .search-popup-info,
  .search-popup-block,
  .side-info-feature,
  .subreview {
    background-color: ${({ theme }) => theme.overlay2Color};
  }

  .hit-item a,
  .modal-content .close {
    color: ${({ theme }) => theme.textColorDark};
  }

  .prereq .ui.button {
    color: ${({ theme }) => theme.prereqNodeTextColor};
    background-color: ${({ theme }) => theme.prereqNodeBackgroundColor};
  }

  `;

  export default GlobalStyle;