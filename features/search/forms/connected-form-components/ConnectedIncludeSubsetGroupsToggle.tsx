import { useDispatch, useSelector } from 'react-redux';
import IncludeSubsetGroupsToggle from '../../../../components/search-form-components/IncludeSubsetGroupsToggle';
import { RootState } from '../../../../redux/rootReducer';
import { ConnectedSearchFormComponentProps } from './types';

interface ConnectedIncludeSubsetGroupsToggleProps extends ConnectedSearchFormComponentProps {
  setIncludeSubsetGroups: any;
}

const ConnectedIncludeSubsetGroupsToggle: React.FC<ConnectedIncludeSubsetGroupsToggleProps> = ({ reduxSlice, setIncludeSubsetGroups }) => {
  const dispatch = useDispatch();
  const { includeSubsetGroups } = useSelector((state: RootState) => state[reduxSlice]);

  const updateIncludeSubsetGroups = (toggleValue) => {
    dispatch(setIncludeSubsetGroups(toggleValue));
  };

  return <IncludeSubsetGroupsToggle includeSubsetGroups={includeSubsetGroups} updateIncludeSubsetGroups={updateIncludeSubsetGroups} />;
};

export default ConnectedIncludeSubsetGroupsToggle;
