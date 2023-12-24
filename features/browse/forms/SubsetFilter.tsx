/* eslint-disable jsx-a11y/no-static-element-interactions */
/* eslint-disable jsx-a11y/click-events-have-key-events */
import FormControl from '@material-ui/core/FormControl';
import InputLabel from '@material-ui/core/InputLabel';
import MenuItem from '@material-ui/core/MenuItem';
import Select from '@material-ui/core/Select';
import React, { useEffect, useState } from 'react';
import { useDispatch, useSelector } from 'react-redux';

interface SubsetFilterProps {
  label: string;
  subsetOptions?: { label: string; value: string }[];
  reduxSlice: string;
  setSubsets: (subsets: string[]) => void;
}

const SubsetFilter: React.FC<SubsetFilterProps> = ({ label, subsetOptions, setSubsets, reduxSlice }) => {
  const dispatch = useDispatch();
  const { subsets } = useSelector((state) => state[reduxSlice]);

  const [selected, setSelected] = useState<string[]>(['All']);
  const [previous, setPrevious] = useState<string[]>(['All']);

  useEffect(
    () => () => {
      dispatch(setSubsets(['All']));
    },
    []
  );

  return (
    <FormControl style={{ maxWidth: '170px' }}>
      <InputLabel id="subset-filter-label">{label}</InputLabel>
      <Select
        labelId="subset-filter-label"
        id="subset-filter"
        label="Subsets"
        value={subsets}
        multiple
        onChange={(e) => {
          const currentValues = e.target.value as string[];
          const allWasJustSelected = currentValues.includes('All') && !previous.includes('All');
          const somethingOtherThanAllWasJustSelected =
            currentValues.length > 1 && currentValues.includes('All') && previous.includes('All');
          const nothingWasSelected = currentValues.length === 0;
          let newValue = [];
          if (allWasJustSelected) {
            newValue = ['All'];
            setSelected(newValue);
            setPrevious(newValue);
          } else if (somethingOtherThanAllWasJustSelected) {
            newValue = currentValues.filter((item: string) => item !== 'All');
            setSelected(newValue);
            setPrevious(newValue);
          } else if (nothingWasSelected) {
            newValue = ['All'];
            setSelected(newValue);
            setPrevious(newValue);
          } else {
            newValue = currentValues;
            setSelected(currentValues);
            setPrevious(currentValues);
          }
          dispatch(setSubsets(newValue));
        }}
        style={{ marginRight: '10px' }}
      >
        {subsetOptions.map((option, index) => (
          <MenuItem key={option.value} value={option.value}>
            {option.label}
          </MenuItem>
        ))}
      </Select>
    </FormControl>
  );
};
export default SubsetFilter;
