import { useDispatch, useSelector } from 'react-redux';
import ShowAllPrintingsToggle from '../../../../components/search-form-components/ShowAllPrintingsToggle';
import { RootState } from '../../../../redux/rootReducer';
import { setShowAllPrintings } from '../../browseSlice';

const ConnectedShowAllPrintingsToggle: React.FC = () => {
  const dispatch = useDispatch();
  const { showAllPrintings } = useSelector((state: RootState) => state.browse);

  const updateShowAllPrintings = (toggleValue) => {
    dispatch(setShowAllPrintings(toggleValue));
  };

  return <ShowAllPrintingsToggle showAllPrintings={showAllPrintings} updateShowAllPrintings={updateShowAllPrintings} />;
};

export default ConnectedShowAllPrintingsToggle;
