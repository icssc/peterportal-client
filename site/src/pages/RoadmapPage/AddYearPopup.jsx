import React, { useState } from "react";
import "./AddYearPopup.scss";
import { Button, Popup, Form, Input } from "semantic-ui-react";

function AddYearPopup({ handleAddYear, setPopUp, popUp }) {
  const placeholderYear = new Date().getFullYear();
  const [year, setYear] = useState(placeholderYear);

  return (
    <Popup
      trigger={
        <Button
          className="add-year-btn"
          icon="plus circle"
          content="Add year"
        />
      }
      open={popUp}
      onOpen={() => setPopUp(true)}
      onClose={() => setPopUp(false)}
      content={
        <Form
          onSubmit={() => {
            handleAddYear(year);
            setYear(placeholderYear);
          }}
        >
          <Form.Field>
            <label>Start Year</label>
            <Input
              type="number"
              name="year"
              value={year}
              onChange={(e) => {
                setYear(e.target.value);
              }}
              min={1000}
              max={9999}
              placeholder={placeholderYear.toString()}
            ></Input>
          </Form.Field>
          <Button className="popup-btn" content="Add Year" />
        </Form>
      }
      on="click"
    />
  );
}

export default AddYearPopup;
