import React from "react";
import { Accordion, Menu } from "semantic-ui-react";
import "./Filter.scss";
import {
  RefinementListFilter,
} from "searchkit";


class ProfessorFilter extends React.Component {
  state = { activeIndex: 0 }; 

  handleClick = (e, titleProps) => {
    const { index } = titleProps;
    const { activeIndex } = this.state;
    const newIndex = activeIndex === index ? -1 : index;

    this.setState({ activeIndex: newIndex });
  };

  render() {
    const { activeIndex } = this.state;
    return (
      <div className="filter-list-container">
        <a href="https://forms.gle/qAhCng7Ygua7SZ358"><h5 style={{margin: 0}}><span role="img" aria-label="thinking face">🤔</span> Can't find your professor?</h5></a>
        <h4>Search Filter</h4>
        <div style={{ overflowY: "auto" }}>
          <Accordion vertical>
            <Menu.Item>
              <Accordion.Title
                active={activeIndex === 0}
                content="School"
                index={0}
                onClick={this.handleClick}
              />
              <Accordion.Content
                active={activeIndex === 0}
                content={
                  <RefinementListFilter
                    id="school"
                    field="schools.keyword"
                    operator="OR"
                  />
                }
              />
            </Menu.Item>

            <Menu.Item>
              <Accordion.Title
                active={activeIndex === 1}
                content="Department"
                index={1}
                onClick={this.handleClick}
              />
              <Accordion.Content
                active={activeIndex === 1}
                content={
                  <RefinementListFilter
                    id="depts"
                    field="department.keyword"
                    operator="OR"
                    size={200}
                  />
                }
              />
            </Menu.Item>
          </Accordion>
        </div>
      </div>
    );
  }
}

export default ProfessorFilter;
