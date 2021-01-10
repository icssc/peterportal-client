import React from "react";
import { Accordion, Menu } from "semantic-ui-react";
import "./Filter.scss";
import { RefinementListFilter, Select, TermQuery, CheckboxFilter, MenuFilter } from "searchkit";

const GEForm = (
  <div>
    <CheckboxFilter title="ge-Ia" id="ge-Ia" label="GE Ia: Lower Division Writing" filter={TermQuery("ge_list.keyword", "GE Ia: Lower Division Writing")} />
    <CheckboxFilter title="ge-Ib" id="ge-Ib" label="GE Ib: Upper Division Writing" filter={TermQuery("ge_list.keyword", "GE Ib: Upper Division Writing")} />
    <CheckboxFilter title="ge-II" id="ge-II" label="GE II: Science and Technology" filter={TermQuery("ge_list.keyword", "GE II: Science and Technology")} />
    <CheckboxFilter title="ge-III" id="ge-III" label="GE III: Social & Behavioral Sciences" filter={TermQuery("ge_list.keyword", "GE III: Social & Behavioral Sciences")} />
    <CheckboxFilter title="ge-IV" id="ge-IV" label="GE IV: Arts and Humanities" filter={TermQuery("ge_list.keyword", "GE IV: Arts and Humanities")} />
    <CheckboxFilter title="ge-Va" id="ge-Va" label="GE Va: Quantitative Literacy" filter={TermQuery("ge_list.keyword", "GE Va: Quantitative Literacy")} />
    <CheckboxFilter title="ge-Vb" id="ge-Vb" label="GE Vb: Formal Reasoning" filter={TermQuery("ge_list.keyword", "GE Vb: Formal Reasoning")} />
    <CheckboxFilter title="ge-VI" id="ge-VI" label="GE VI: Language Other Than English" filter={TermQuery("ge_list.keyword", "GE VI: Language Other Than English")} />
    <CheckboxFilter title="ge-VII" id="ge-VII" label="GE VII: Multicultural Studies" filter={TermQuery("ge_list.keyword", "GE VII: Multicultural Studies")} />
    <CheckboxFilter title="ge-VII" id="ge-VIII" label="GE VIII: International/Global Issues" filter={TermQuery("ge_list.keyword", "GE VIII: International/Global Issues")} />
  </div>
)

const CourseLevelForm = (
  <div>
    <CheckboxFilter title="course-level-lower" id="course-level-lower" label="Lower Division (1-99)" filter={TermQuery("course_level.keyword", "Lower Division (1-99)")} />
    <CheckboxFilter title="course-level-upper" id="course-level-upper" label="Upper Division (100-199)" filter={TermQuery("course_level.keyword", "Upper Division (100-199)")} />
    <CheckboxFilter title="course-level-grad" id="course-level-grad" label="Graduate/Professional Only (200+)" filter={TermQuery("course_level.keyword", "Graduate/Professional Only (200+)")} />
  </div>
)

export default class CourseFilter extends React.Component {
  state = {
    activeIndex: 0,
    terms: []
  }

  constructor(props) {
    super(props);
    this.getTerms();
  }

  getTerms = () => {
    fetch(`/api/v1/schedule/getTerms`)
      .then(res => res.json())
      .then(terms => {
        // terms.reverse();
        this.setState({ terms: terms })
      });
  }

  handleClick = (e, titleProps) => {
    const { index } = titleProps
    const { activeIndex } = this.state
    const newIndex = activeIndex === index ? -1 : index

    this.setState({ activeIndex: newIndex })
  }

  render() {
    const { activeIndex } = this.state;

    return (
      <div className="filter-list-container">
        <h4>Search Filter</h4>
        <div style={{ overflowY: "auto" }}>
          <Accordion>
          <Menu.Item>
              <Accordion.Title active={activeIndex === 0}
                content='Term Offered'
                index={0}
                onClick={this.handleClick}
              />
              <Accordion.Content active={activeIndex === 0} content={<MenuFilter field="terms.keyword" id="terms" listComponent={Select}/>} />
            </Menu.Item>

            <Menu.Item>
              <Accordion.Title
                active={activeIndex === 1}
                content='General Education'
                index={1}
                onClick={this.handleClick}
              />
              <Accordion.Content active={activeIndex === 1} content={GEForm} />
            </Menu.Item>

            <Menu.Item>
              <Accordion.Title
                active={activeIndex === 2}
                content='Course Level'
                index={2}
                onClick={this.handleClick}
              />
              <Accordion.Content active={activeIndex === 2} content={CourseLevelForm} />
            </Menu.Item>

            <Menu.Item>
              <Accordion.Title
                active={activeIndex === 3}
                content='School'
                index={3}
                onClick={this.handleClick}
              />
              <Accordion.Content active={activeIndex === 3} content={<RefinementListFilter id="school" field="school.keyword" operator="OR" title="" />} />
            </Menu.Item>

            <Menu.Item>
              <Accordion.Title
                active={activeIndex === 4}
                content='Department'
                index={4}
                onClick={this.handleClick}
              />
              <Accordion.Content active={activeIndex === 4} content={<RefinementListFilter id="depts" field="department.keyword" operator="OR" size={200} title="" />} />
            </Menu.Item>
          </Accordion>
        </div>

      </div>

    )
  }
}

